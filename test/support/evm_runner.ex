defmodule Logos.Conformance.EVMRunner do
  @moduledoc """
  Deploys bytecode to a local Anvil node and reads back code, balances, and call results.

  The node is Shanghai, so PUSH0 is legal. Nothing here talks to a public network.
  """

  alias Logos.Conformance.{ABI, Tooling}

  def run_scenario(scenario, _timeout \\ 5_000) do
    cond do
      not Tooling.anvil?() ->
        %{"status" => "not_run", "reason" => "anvil NOT_INSTALLED"}

      scenario != "push0_runtime" and not Tooling.solc?() ->
        %{"status" => "not_run", "reason" => "solc NOT_INSTALLED"}

      true ->
        try do
          %{url: url, from: from} = ensure_node!()
          execute(scenario, url, from)
        rescue
          exception ->
            %{
              "status" => "error",
              "exception" => exception.__struct__ |> Module.split() |> List.last(),
              "message" => Exception.message(exception)
            }
        end
    end
  end

  defp execute("push0_runtime", url, from) do
    runtime = <<0x5F, 0x5F, 0xF3>>
    creation = creation_for(runtime)

    with {:ok, address} <- deploy(url, from, creation) do
      code = get_code(url, address)

      %{
        "status" => "ok",
        "code" => code,
        "runtime" => "0x" <> Base.encode16(runtime, case: :lower)
      }
    else
      {:error, reason} -> %{"status" => "error", "message" => reason}
    end
  end

  defp execute("ledger_transfer", url, from) do
    with {:ok, bins} <- compile_fixtures(),
         {:ok, token} <- deploy(url, from, bin!(bins, "LedgerToken")),
         bal_before <- balance_of(url, from, token, from),
         {:error, _} <-
           call(url, from, token, ABI.encode_transfer_from(from, zero_address(), 1)),
         {:ok, [_, recipient | _]} <- rpc(url, "eth_accounts", []),
         {:ok, _} <- transact(url, from, token, encode_approve(from, 50)),
         {:ok, returned} <-
           call(url, from, token, ABI.encode_transfer_from(from, recipient, 50)),
         {:ok, _} <-
           transact(url, from, token, ABI.encode_transfer_from(from, recipient, 50)),
         bal_after <- balance_of(url, from, token, from),
         bal_recipient <- balance_of(url, from, token, recipient) do
      %{
        "status" => "ok",
        "balance_before" => bal_before,
        "balance_after" => bal_after,
        "recipient_balance" => bal_recipient,
        "return_word" => returned,
        "unauthorized_reverted" => true
      }
    else
      {:error, reason} -> %{"status" => "error", "message" => inspect(reason)}
    end
  end

  defp execute("ledger_zero_amount", url, from) do
    ledger_rejected(url, from, 50, 0)
  end

  defp execute("ledger_insufficient_allowance", url, from) do
    ledger_rejected(url, from, 1, 50)
  end

  defp execute("ledger_insufficient_balance", url, from) do
    ledger_rejected(url, from, 5_000, 1_001)
  end

  defp execute("token_" <> kind, url, from) do
    contract = token_contract(kind)

    with {:ok, bins} <- compile_fixtures(),
         {:ok, token} <- deploy(url, from, bin!(bins, contract)),
         result <- call(url, from, token, ABI.encode_transfer_from(from, from, 1)) do
      case result do
        {:ok, data} ->
          %{"status" => "ok", "return_hex" => data, "reverted" => false}

        {:error, message} ->
          %{"status" => "ok", "reverted" => true, "revert_message" => message}
      end
    else
      {:error, reason} -> %{"status" => "error", "message" => inspect(reason)}
    end
  end

  defp execute("set_emit", url, from) do
    source = """
    Contract C
    State
      record { x : Int = 0 }
    Clauses
      Clause B(): Effect
        Provided That
          x == 0
        Shall
          All [ Set { field: "x", value: 1 }, Emit { event: "Ping", data: {} } ]
    End
    """

    with {:ok, solidity} <- safe_solidity(source),
         {:ok, bins} <- compile_source(solidity),
         {:ok, address} <- deploy(url, from, bin!(bins, "C")),
         {:ok, receipt} <- transact(url, from, address, ABI.selector("B()")),
         {:ok, stored} <- call(url, from, address, ABI.selector("x()")) do
      logs = receipt["logs"] || []

      %{
        "status" => "ok",
        "storage_x" => word_decimal(stored),
        "gas_used" => word_decimal(receipt["gasUsed"]),
        "bytecode_bytes" => Integer.to_string(byte_size(bin!(bins, "C"))),
        "log_count" => Integer.to_string(length(logs)),
        "topic0" => topic0(logs)
      }
    else
      {:error, reason} -> %{"status" => "error", "message" => inspect(reason)}
    end
  end

  defp execute("eoa_call", url, from) do
    # An EOA has no code. A successful CALL is not an ERC-20 transfer.
    {:ok, data} = call(url, from, from, ABI.encode_transfer_from(from, from, 1))

    %{
      "status" => "ok",
      "return_hex" => data,
      "code" => get_code(url, from)
    }
  end

  defp execute("reenter", url, from) do
    with {:ok, bins} <- compile_fixtures(),
         {:ok, victim} <- deploy(url, from, bin!(bins, "ReentrantVictim")),
         {:ok, token} <- deploy(url, from, bin!(bins, "CallbackToken")),
         {:ok, _} <- transact(url, from, victim, encode_pull(token)),
         calls <- calls_of(url, from, victim) do
      %{"status" => "ok", "callback_count" => calls}
    else
      {:error, reason} -> %{"status" => "error", "message" => inspect(reason)}
    end
  end

  defp ledger_rejected(url, from, allowance, amount) do
    with {:ok, bins} <- compile_fixtures(),
         {:ok, token} <- deploy(url, from, bin!(bins, "LedgerToken")),
         {:ok, [_, recipient | _]} <- rpc(url, "eth_accounts", []),
         {:ok, _} <- transact(url, from, token, encode_approve(from, allowance)),
         before <- balance_of(url, from, token, from),
         {:error, message} <-
           call(url, from, token, ABI.encode_transfer_from(from, recipient, amount)),
         after_bal <- balance_of(url, from, token, from) do
      %{
        "status" => "ok",
        "reverted" => true,
        "balance_before" => before,
        "balance_after" => after_bal,
        "revert_message" => message
      }
    else
      {:ok, data} -> %{"status" => "error", "message" => "expected revert, got #{data}"}
      {:error, reason} -> %{"status" => "error", "message" => inspect(reason)}
    end
  end

  defp topic0([%{"topics" => [first | _]} | _]), do: String.downcase(first)
  defp topic0(_), do: "0x"

  defp token_contract("true"), do: "TokenTrue"
  defp token_contract("false"), do: "TokenFalse"
  defp token_contract("empty"), do: "TokenEmpty"
  defp token_contract("revert"), do: "TokenRevert"
  defp token_contract("one_byte"), do: "TokenOneByte"
  defp token_contract("thirty_one"), do: "TokenThirtyOne"
  defp token_contract("thirty_three"), do: "TokenThirtyThree"
  defp token_contract("dirty_bool"), do: "TokenDirtyBool"

  defp creation_for(runtime) when byte_size(runtime) < 256 do
    # PUSH1 size, PUSH1 offset, PUSH0, CODECOPY, PUSH1 size, PUSH0, RETURN.
    # RETURN is not in the Logos assembler table, so this prefix is raw bytes.
    # The prefix is 10 bytes, so the runtime blob starts at offset 10.
    size = byte_size(runtime)
    <<0x60, size, 0x60, 10, 0x5F, 0x39, 0x60, size, 0x5F, 0xF3, runtime::binary>>
  end

  defp compile_fixtures do
    path =
      Path.join(Logos.Conformance.CorpusRunner.root(), "conformance/fixtures/erc20/Doubles.sol")

    compile_source(File.read!(path))
  end

  defp compile_source(source) do
    dir = Path.join(System.tmp_dir!(), "logos-solc-#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    input = Path.join(dir, "Doubles.sol")
    File.write!(input, source)
    solc = Tooling.solc_path()

    case System.cmd(solc, ["--bin", "--evm-version", "shanghai", input], stderr_to_stdout: true) do
      {out, 0} -> {:ok, parse_bins(out)}
      {out, _} -> {:error, String.trim(out)}
    end
  end

  defp parse_bins(output) do
    Regex.scan(~r/======= .*?:(\w+) =======\nBinary:\n([0-9a-fA-F]*)/, output)
    |> Map.new(fn [_, name, hex] -> {name, hex} end)
  end

  defp bin!(bins, name) do
    case Map.fetch(bins, name) do
      {:ok, hex} when hex != "" ->
        {:ok, bytes} = Base.decode16(hex, case: :mixed)
        bytes

      _ ->
        raise "solc did not emit bytecode for #{name}"
    end
  end

  defp deploy(url, from, bytecode) do
    data = "0x" <> Base.encode16(bytecode, case: :lower)

    case rpc(url, "eth_sendTransaction", [%{"from" => from, "data" => data}]) do
      {:ok, hash} ->
        case receipt(url, hash) do
          %{"status" => "0x1", "contractAddress" => address} -> {:ok, address}
          receipt -> {:error, "deploy failed #{inspect(receipt)}"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp safe_solidity(source) do
    try do
      {:ok, Logos.Compiler.compile_to_solidity(source)}
    rescue
      exception -> {:error, Exception.message(exception)}
    end
  end

  defp transact(url, from, to, data) do
    hex = "0x" <> Base.encode16(data, case: :lower)

    case rpc(url, "eth_sendTransaction", [%{"from" => from, "to" => to, "data" => hex}]) do
      {:ok, hash} ->
        case receipt(url, hash) do
          %{"status" => "0x1"} = receipt -> {:ok, receipt}
          %{"status" => "0x0"} = failed -> {:error, "reverted #{inspect(failed)}"}
          other -> {:error, inspect(other)}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp word_decimal("0x" <> hex), do: String.to_integer(hex, 16) |> Integer.to_string()

  defp call(url, from, to, data) do
    hex = "0x" <> Base.encode16(data, case: :lower)

    case rpc(url, "eth_call", [%{"from" => from, "to" => to, "data" => hex}, "latest"]) do
      {:ok, result} -> {:ok, String.downcase(result)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp get_code(url, address) do
    {:ok, code} = rpc(url, "eth_getCode", [address, "latest"])
    String.downcase(code)
  end

  defp balance_of(url, from, token, account) do
    selector = ABI.selector("balanceOf(address)")
    {:ok, word} = call(url, from, token, selector <> ABI.word_address(account))
    word |> String.trim_leading("0x") |> String.to_integer(16) |> Integer.to_string()
  end

  defp calls_of(url, from, victim) do
    selector = ABI.selector("calls()")
    {:ok, word} = call(url, from, victim, selector)
    word |> String.trim_leading("0x") |> String.to_integer(16) |> Integer.to_string()
  end

  defp encode_approve(spender, amount) do
    ABI.selector("approve(address,uint256)") <>
      ABI.word_address(spender) <> ABI.word_uint(amount)
  end

  defp encode_pull(token) do
    ABI.selector("pull(address)") <> ABI.word_address(token)
  end

  defp zero_address, do: "0x0000000000000000000000000000000000000000"

  defp ensure_node! do
    case :persistent_term.get({__MODULE__, :node}, nil) do
      nil -> start_node!()
      info -> info
    end
  end

  defp start_node! do
    :inets.start()
    :ssl.start()
    port = free_port()
    anvil = Tooling.anvil_path()

    port_ref =
      Port.open({:spawn_executable, anvil}, [
        :binary,
        :exit_status,
        :hide,
        :stderr_to_stdout,
        args: [
          "--hardfork",
          Tooling.anvil_hardfork(),
          "--port",
          Integer.to_string(port),
          "--silent"
        ]
      ])

    url = "http://127.0.0.1:#{port}"
    wait_until!(fn -> match?({:ok, _}, rpc(url, "eth_blockNumber", [])) end)
    {:ok, accounts} = rpc(url, "eth_accounts", [])
    info = %{port: port_ref, url: url, from: hd(accounts)}
    :persistent_term.put({__MODULE__, :node}, info)
    info
  end

  defp free_port do
    {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)
    port
  end

  defp wait_until!(fun, attempts \\ 50)
  defp wait_until!(_fun, 0), do: raise("anvil did not become ready")

  defp wait_until!(fun, attempts) do
    if fun.() do
      :ok
    else
      Process.sleep(100)
      wait_until!(fun, attempts - 1)
    end
  end

  defp receipt(url, hash, attempts \\ 40)
  defp receipt(_url, hash, 0), do: raise("timed out waiting for receipt #{hash}")

  defp receipt(url, hash, attempts) do
    case rpc(url, "eth_getTransactionReceipt", [hash]) do
      {:ok, nil} ->
        Process.sleep(50)
        receipt(url, hash, attempts - 1)

      {:ok, receipt} ->
        receipt

      {:error, reason} ->
        raise "receipt error #{reason}"
    end
  end

  defp rpc(url, method, params) do
    body =
      JSON.encode!(%{
        "jsonrpc" => "2.0",
        "id" => 1,
        "method" => method,
        "params" => params
      })

    request = {String.to_charlist(url), [], ~c"application/json", body}

    case :httpc.request(:post, request, [{:timeout, 5_000}], []) do
      {:ok, {{_, 200, _}, _headers, response}} ->
        decoded = JSON.decode!(to_string(response))

        cond do
          is_map(decoded["error"]) ->
            {:error, decoded["error"]["message"] || inspect(decoded["error"])}

          Map.has_key?(decoded, "result") ->
            {:ok, decoded["result"]}

          true ->
            {:error, inspect(decoded)}
        end

      {:error, reason} ->
        {:error, inspect(reason)}

      other ->
        {:error, inspect(other)}
    end
  end
end
