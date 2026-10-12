defmodule Logos.Conformance.CorpusRunner do
  @moduledoc """
  Loads the portable JSON corpus and classifies each case.

  `PASS`, `KNOWN_FAILURE`, `BLOCKED`, `SKIPPED`, and `NOT_RUN` keep the suite
  green. `FAIL`, `TIMEOUT`, and `UNEXPECTED_PASS` fail it. A known failure that
  starts passing is `UNEXPECTED_PASS`, so a bugfix has to update the registry.
  """

  alias Logos.Conformance.{ABI, Keccak, Normalization, Tooling, Watchdog}

  @root Path.expand("../..", __DIR__)
  @allowed ~w(PASS KNOWN_FAILURE BLOCKED SKIPPED NOT_RUN)
  @classes ~w(current spec parity)
  @origins ~w(ethereum_standard logos_spec reviewed_reference observed_elixir)
  @priorities ~w(P0 P1 P2)
  @required ~w(
    id title category priority tags class source_ref rationale origin
    implementation_status fixture_version baseline_commit phases
  )

  def root, do: @root
  def allowed_statuses, do: @allowed

  def load_registry do
    path = Path.join(@root, "conformance/known_failures.json")
    JSON.decode!(File.read!(path))
  end

  def load_cases do
    Path.join(@root, "conformance/cases/**/*.json")
    |> Path.wildcard()
    |> Enum.sort()
    |> Enum.flat_map(&decode_file/1)
  end

  def run_all(cases \\ load_cases()) do
    registry = load_registry()
    Enum.map(cases, &run(&1, registry))
  end

  def run(case, registry \\ load_registry()) do
    started = System.monotonic_time(:millisecond)

    outcome =
      cond do
        schema_error = validate(case, registry) ->
          %{
            "status" => "FAIL",
            "reason" => "schema",
            "message" => schema_error
          }

        case["implementation_status"] == "spec_gap" ->
          %{
            "status" => "SKIPPED",
            "reason" => "SPEC_GAP",
            "note" => case["note"]
          }

        blocker_active?(case["blocked_by"]) ->
          %{
            "status" => "BLOCKED",
            "blocked_by" => case["blocked_by"]
          }

        true ->
          actual = execute(case)
          classify(case, actual)
      end

    elapsed = System.monotonic_time(:millisecond) - started

    Map.merge(outcome, %{
      "id" => case["id"],
      "class" => case["class"],
      "priority" => case["priority"],
      "known_failure_id" => case["known_failure_id"],
      "elapsed_ms" => elapsed
    })
  end

  def write_report(results, versions \\ Tooling.versions()) do
    path = Path.join(@root, "reports/last-result.json")
    File.mkdir_p!(Path.dirname(path))

    body = %{
      "fixture_version" => 1,
      "generated_at" => DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601(),
      "versions" => versions,
      "counts" => counts(results),
      "results" => results
    }

    File.write!(path, Normalization.encode(body) <> "\n")
    path
  end

  def counts(results) do
    Enum.reduce(results, %{}, fn result, acc ->
      Map.update(acc, result["status"], 1, &(&1 + 1))
    end)
  end

  def suite_failures(results) do
    Enum.reject(results, &(&1["status"] in @allowed))
  end

  defp decode_file(path) do
    decoded = JSON.decode!(File.read!(path))

    cases =
      cond do
        is_list(decoded) -> decoded
        is_map(decoded) and is_list(decoded["cases"]) -> decoded["cases"]
        is_map(decoded) -> [decoded]
      end

    Enum.map(cases, &Map.put(&1, "_file", Path.relative_to(path, @root)))
  end

  defp validate(case, registry) do
    missing = Enum.reject(@required, &Map.has_key?(case, &1))

    cond do
      missing != [] ->
        "missing keys #{Enum.join(missing, ", ")}"

      case["class"] not in @classes ->
        "bad class"

      case["origin"] not in @origins ->
        "bad origin"

      case["priority"] not in @priorities ->
        "bad priority"

      case["fixture_version"] != 1 ->
        "fixture_version must be 1"

      not is_list(case["phases"]) or case["phases"] == [] ->
        "phases must be a non-empty list"

      not Regex.match?(~r/^[0-9a-f]{40}$/, case["baseline_commit"] || "") ->
        "baseline_commit must be a 40-char git sha"

      id = case["known_failure_id"] ->
        if registry_has?(registry, id), do: nil, else: "unknown known_failure_id #{id}"

      true ->
        nil
    end
  end

  defp registry_has?(registry, id) do
    Enum.any?(registry["failures"], &(&1["id"] == id))
  end

  defp execute(case) do
    timeout = case["timeout_ms"] || 2_000

    case["phases"]
    |> Enum.reduce(%{}, fn phase, acc ->
      Map.put(acc, phase, run_phase(phase, case, timeout))
    end)
    |> Normalization.canonicalize()
  end

  defp run_phase("parse", case, timeout), do: parse_phase(source_of(case), timeout)
  defp run_phase("solidity", case, timeout), do: solidity_phase(source_of(case), timeout)
  defp run_phase("evm", case, timeout), do: evm_phase(source_of(case), timeout)
  defp run_phase("assemble", case, timeout), do: assemble_phase(case["instructions"], timeout)
  defp run_phase("keccak", case, _timeout), do: keccak_phase(case)
  defp run_phase("abi", case, _timeout), do: abi_phase(case)
  defp run_phase("atoms", case, timeout), do: atoms_phase(case, timeout)

  defp run_phase("anvil", case, timeout) do
    Logos.Conformance.EVMRunner.run_scenario(case["scenario"], timeout)
  end

  defp run_phase(other, _case, _timeout) do
    %{"status" => "error", "exception" => "CorpusRunner", "message" => "unknown phase #{other}"}
  end

  defp parse_phase(source, timeout) do
    case Watchdog.run(fn -> Logos.Parser.parse(source) end, timeout) do
      {:ok, ast} -> %{"status" => "ok", "ast" => Normalization.ast(ast)}
      {:error, module, message} -> Normalization.exception(module, message)
      {:timeout, ms} -> %{"status" => "timeout", "timeout_ms" => ms}
    end
  end

  defp solidity_phase(source, timeout) do
    case Watchdog.run(fn -> Logos.Compiler.compile_to_solidity(source) end, timeout) do
      {:ok, solidity} -> %{"status" => "ok", "source" => solidity}
      {:error, module, message} -> Normalization.exception(module, message)
      {:timeout, ms} -> %{"status" => "timeout", "timeout_ms" => ms}
    end
  end

  defp evm_phase(source, timeout) do
    case Watchdog.run(fn -> Logos.Compiler.compile_to_evm(source) end, timeout) do
      {:ok, %{runtime_hex: runtime, creation_hex: creation}} ->
        %{
          "status" => "ok",
          "runtime_hex" => runtime,
          "creation_hex" => creation
        }

      {:error, module, message} ->
        Normalization.exception(module, message)

      {:timeout, ms} ->
        %{"status" => "timeout", "timeout_ms" => ms}
    end
  end

  defp assemble_phase(instructions, timeout) do
    instrs = decode_instructions(instructions)

    case Watchdog.run(fn -> Logos.EVM.Assembler.assemble(instrs) end, timeout) do
      {:ok, bin} ->
        %{
          "status" => "ok",
          "hex" => "0x" <> Base.encode16(bin, case: :lower),
          "byte_size" => byte_size(bin)
        }

      {:error, module, message} ->
        Normalization.exception(module, message)

      {:timeout, ms} ->
        %{"status" => "timeout", "timeout_ms" => ms}
    end
  end

  defp keccak_phase(case) do
    input = case["keccak_input"]
    signature = case["signature"]

    %{
      "status" => "ok",
      "hex" => "0x" <> Keccak.hash_hex(input),
      "sha3_hex" => "0x" <> Base.encode16(Keccak.sha3_256(input), case: :lower),
      "selector" => if(signature, do: "0x" <> Keccak.selector_hex(signature), else: nil)
    }
  end

  defp abi_phase(%{"abi_kind" => "transfer_from"} = case) do
    encoded =
      ABI.encode_transfer_from(case["from"], case["to"], parse_int(case["amount"]))

    %{
      "status" => "ok",
      "hex" => "0x" <> Base.encode16(encoded, case: :lower),
      "byte_size" => byte_size(encoded)
    }
  end

  defp abi_phase(%{"abi_kind" => "bool_word", "value" => value}) do
    word =
      case value do
        true -> ABI.word_bool(true)
        false -> ABI.word_bool(false)
        "dirty" -> <<2::unsigned-big-integer-size(256)>>
        "short" -> <<1>>
      end

    %{
      "status" => "ok",
      "valid" => ABI.valid_bool_word?(word),
      "byte_size" => byte_size(word)
    }
  end

  defp abi_phase(%{"abi_kind" => "address_word", "value" => value}) do
    word =
      case value do
        "clean" -> ABI.word_address("0x1111111111111111111111111111111111111111")
        "dirty" -> <<1, 0::unsigned-size(248)>>
      end

    %{
      "status" => "ok",
      "clean" => ABI.clean_address_word?(word)
    }
  end

  defp abi_phase(%{"abi_kind" => "calldata_bounds", "byte_size" => size, "args" => args}) do
    %{
      "status" => "ok",
      "complete" => ABI.calldata_complete?(:binary.copy(<<0>>, size), args)
    }
  end

  defp atoms_phase(%{"atom_target" => "parser"}, _timeout) do
    before = :erlang.system_info(:atom_count)

    Enum.each(1..40, fn index ->
      _ =
        Logos.Parser.parse(
          unique_contract("field_#{index}_#{System.unique_integer([:positive])}")
        )
    end)

    %{"status" => "ok", "growth" => :erlang.system_info(:atom_count) - before}
  end

  defp atoms_phase(%{"atom_target" => "evm"}, timeout) do
    before = :erlang.system_info(:atom_count)

    Enum.each(1..15, fn index ->
      source = unique_contract("Clause#{index}#{System.unique_integer([:positive])}")

      case Watchdog.run(fn -> Logos.Compiler.compile_to_evm(source) end, timeout) do
        _ -> :ok
      end
    end)

    %{"status" => "ok", "growth" => :erlang.system_info(:atom_count) - before}
  end

  defp source_of(case) do
    cond do
      is_binary(case["logos_source"]) ->
        case["logos_source"]

      is_binary(case["logos_source_hex"]) ->
        {:ok, bin} = Base.decode16(case["logos_source_hex"], case: :mixed)
        bin

      true ->
        nil
    end
  end

  defp unique_contract(clause) do
    """
    Contract C
    State
      record { x : Int = 0 }
    Clauses
      Clause #{clause}(): Effect
        Provided That
          x == 0
        Shall
          Set { field: "x", value: 1 }
    End
    """
  end

  defp decode_instructions(instructions) do
    Enum.map(instructions, &decode_instruction/1)
  end

  defp decode_instruction(%{"op" => op}), do: {:op, String.to_atom(op)}
  defp decode_instruction(%{"label" => name}), do: {:label, String.to_atom(name)}
  defp decode_instruction(%{"jump_label" => name}), do: {:jump_label, String.to_atom(name)}
  defp decode_instruction(%{"jumpi_label" => name}), do: {:jumpi_label, String.to_atom(name)}

  defp decode_instruction(%{"push_u" => value}) when is_binary(value) do
    {:push_u, parse_int(value)}
  end

  defp decode_instruction(%{"push_u" => value}) when is_integer(value), do: {:push_u, value}

  defp decode_instruction(%{"push" => hex}) do
    {:ok, bin} = Base.decode16(String.trim_leading(hex, "0x"), case: :mixed)
    {:push, bin}
  end

  defp decode_instruction(%{"raw_byte" => hex}) do
    {:ok, bin} = Base.decode16(hex, case: :mixed)
    bin
  end

  defp decode_instruction(%{"nested" => list}) when is_list(list) do
    decode_instructions(list)
  end

  defp parse_int(value) when is_integer(value), do: value
  defp parse_int(value) when is_binary(value), do: String.to_integer(value)

  defp classify(case, actual) do
    cond do
      not_run?(actual) ->
        %{"status" => "NOT_RUN", "actual" => actual}

      case["class"] == "current" and matches?(case["expect"], actual) ->
        %{"status" => "PASS", "actual" => actual}

      case["class"] == "current" ->
        failure("FAIL", case, actual)

      matches?(case["expect"], actual) and is_binary(case["known_failure_id"]) ->
        failure("UNEXPECTED_PASS", case, actual)

      matches?(case["expect"], actual) ->
        %{"status" => "PASS", "actual" => actual}

      is_binary(case["known_failure_id"]) and matches?(case["observed"], actual) ->
        %{
          "status" => "KNOWN_FAILURE",
          "actual" => actual,
          "known_failure_id" => case["known_failure_id"]
        }

      timed_out?(actual) ->
        failure("TIMEOUT", case, actual)

      true ->
        failure("FAIL", case, actual)
    end
  end

  defp failure(status, case, actual) do
    %{
      "status" => status,
      "actual" => actual,
      "expect" => case["expect"],
      "observed" => case["observed"],
      "known_failure_id" => case["known_failure_id"]
    }
  end

  defp not_run?(actual) when is_map(actual) do
    Enum.any?(actual, fn
      {_phase, %{"status" => "not_run"}} -> true
      _ -> false
    end)
  end

  defp timed_out?(actual) when is_map(actual) do
    Enum.any?(actual, fn
      {_phase, %{"status" => "timeout"}} -> true
      _ -> false
    end)
  end

  defp matches?(nil, _actual), do: false

  defp matches?(expect, actual) when is_map(expect) do
    expect = Map.drop(expect, ["execute"])
    value_match?(Normalization.canonicalize(expect), Normalization.canonicalize(actual))
  end

  defp matches?(expect, actual) do
    value_match?(Normalization.canonicalize(expect), Normalization.canonicalize(actual))
  end

  defp value_match?(expect, actual) when is_map(expect) and is_map(actual) do
    {special, rest} = Map.split(expect, ~w(contains not_contains message_contains min_growth))

    special_ok?(special, actual) and
      Enum.all?(rest, fn {key, value} ->
        Map.has_key?(actual, key) and value_match?(value, actual[key])
      end)
  end

  defp value_match?(expect, actual) when is_list(expect) and is_list(actual) do
    length(expect) == length(actual) and
      Enum.zip(expect, actual) |> Enum.all?(fn {left, right} -> value_match?(left, right) end)
  end

  defp value_match?(expect, actual), do: expect == actual

  defp special_ok?(special, actual) do
    contains_ok?(special["contains"], actual) and
      not_contains_ok?(special["not_contains"], actual) and
      message_contains_ok?(special["message_contains"], actual) and
      min_growth_ok?(special["min_growth"], actual)
  end

  defp contains_ok?(nil, _actual), do: true

  defp contains_ok?(needles, actual) do
    source =
      actual["source"] || actual["hex"] || actual["return_hex"] || actual["message"] || ""

    Enum.all?(needles, &String.contains?(source, &1))
  end

  defp not_contains_ok?(nil, _actual), do: true

  defp not_contains_ok?(needles, actual) do
    source = actual["source"] || actual["hex"] || ""
    Enum.all?(needles, &(not String.contains?(source, &1)))
  end

  defp message_contains_ok?(nil, _actual), do: true

  defp message_contains_ok?(needle, actual) do
    blob = (actual["message"] || "") <> (actual["revert_message"] || "")
    String.contains?(blob, needle)
  end

  defp min_growth_ok?(nil, _actual), do: true

  defp min_growth_ok?(minimum, actual) do
    String.to_integer(actual["growth"]) >= String.to_integer(minimum)
  end

  defp blocker_active?(nil), do: false

  defp blocker_active?(id) when is_binary(id) do
    key = {:logos_blocker, id}

    case Process.get(key) do
      nil ->
        active = compute_blocker(id)
        Process.put(key, active)
        active

      active ->
        active
    end
  end

  defp compute_blocker("KF-EVM-UNKNOWN-SHR"), do: shr_blocker?()
  defp compute_blocker("KF-ASM-LEFT-PAD"), do: left_pad_blocker?()
  defp compute_blocker(_other), do: false

  defp shr_blocker? do
    source = """
    Contract C
    State
      record { x : Int = 0 }
    Clauses
      Clause A(): Effect
        Provided That
          x == 0
        Shall
          Set { field: "x", value: 1 }
    End
    """

    case Watchdog.run(fn -> Logos.Compiler.compile_to_evm(source) end, 2_000) do
      {:error, RuntimeError, "Unknown opcode: :SHR"} -> true
      _ -> false
    end
  end

  defp left_pad_blocker? do
    case Watchdog.run(
           fn ->
             Logos.EVM.Assembler.assemble([
               {:push_u, 1},
               {:jump_label, :L},
               {:label, :L},
               {:op, :STOP}
             ])
           end,
           400
         ) do
      {:timeout, _} -> true
      _ -> false
    end
  end
end
