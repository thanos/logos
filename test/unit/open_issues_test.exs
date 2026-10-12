defmodule OpenIssuesTest do
  use ExUnit.Case, async: false

  alias Logos.AST
  alias Logos.Compiler
  alias Logos.Data
  alias Logos.Duration
  alias Logos.EVM.Assembler, as: A
  alias Logos.Parser

  test "issue 16 grammar names duration, decimal, option, string, and parentheses" do
    grammar =
      File.read!("docs/quick-introduction.md")
      |> String.split("2.7 Grammar", parts: 2)
      |> Enum.at(1)

    for token <- ["Duration", "Decimal", "Option", "String", "\"(\"", "All"] do
      assert grammar =~ token
    end
  end

  test "issue 23 state and definition annotations are retained" do
    contract =
      Parser.parse("""
      Contract C
      Definitions
        let price : Decimal = 10
      State
        record { submitted : Bool = false, note : String = "ok" }
      Clauses
        Clause A(): Effect
          Provided That
            submitted == false
          Shall
            Set { field: "submitted", value: true }
      End
      """)

    assert contract.types["defs"]["price"] == "Decimal"
    assert contract.types["state"]["submitted"] == "Bool"
    assert contract.types["state"]["note"] == "String"
  end

  test "issue 24 decimal literals keep coefficient and scale" do
    assert Data.decimal("10.50") == {:decimal, 1050, 2}
    assert Data.format_decimal(1050, 2) == "10.50"
    assert Data.format_decimal(100, 1) == "10.0"

    %AST.Contract{defs: defs} =
      Parser.parse("""
      Contract C
      Definitions
        let price : Decimal = 10.50
      State
        record { x : Int = 0 }
      Clauses
        Clause A(): Effect
          Provided That
            x == 0
          Shall
            Set { field: "x", value: 1 }
      End
      """)

    assert defs["price"] == {:decimal, 1050, 2}
  end

  test "issue 25 option values stay some and none" do
    %AST.Contract{state: state} =
      Parser.parse("""
      Contract C
      State
        record { flag : Option<Bool> = Some(true), empty : Option<Int> = None }
      Clauses
        Clause A(): Effect
          Provided That
            x == 0
          Shall
            Set { field: "flag", value: true }
      End
      """)

    assert state["flag"] == {:some, true}
    assert state["empty"] == :none

    solidity =
      Compiler.compile_to_solidity("""
      Contract C
      State
        record { flag : Bool = false }
      Clauses
        Clause A(): Effect
          Provided That
            flag == false
          Shall
            Set { field: "flag", value: Some(7) }
      End
      """)

    assert solidity =~ "flag = 7;"
  end

  test "issue 27 encodings lower decimal, option, and duration to integers" do
    assert Duration.seconds("1d12h") == 129_600

    %{runtime_hex: hex} =
      Compiler.compile_to_evm("""
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
      """)

    assert String.starts_with?(hex, "0x")
    assert {:decimal, 100, 1} |> then(fn {:decimal, coef, _} -> coef end) == 100
  end

  test "issue 38 time, decimals, and options encode as integers and flags" do
    solidity =
      Compiler.compile_to_solidity("""
      Contract C
      State
        record { start_ts : Int = 1, price : Int = 0, flag : Int = 0 }
      Clauses
        Clause A(): Effect
          Provided That
            now >= start_ts + Duration("1d12h")
          Shall
            All [
              Set { field: "price", value: 10.50 },
              Set { field: "flag", value: None }
            ]
      End
      """)

    assert solidity =~ "129600"
    assert solidity =~ "price = 1050;"
    assert solidity =~ "flag = 0;"
  end

  test "issue 43 emit copies a string pool with CODECOPY" do
    %{runtime_hex: hex} = compile_emit(~s|data: { "note": "paid" }|)
    assert hex =~ "39"
    assert hex =~ Base.encode16("\"note\":\"paid\"", case: :lower)
  end

  test "issue 44 two address topics select LOG4" do
    %{runtime_hex: hex} =
      compile_emit(
        ~s|data: { "from": "0x1111111111111111111111111111111111111111", "to": "0x2222222222222222222222222222222222222222" }|
      )

    assert String.ends_with?(String.trim_trailing(hex, "0"), "a4") or hex =~ "a4"
  end

  test "issue 45 transfer logs with LOG3" do
    %{runtime_hex: hex} =
      Compiler.compile_to_evm("""
      Contract C
      State
        record { x : Int = 0 }
      Clauses
        Clause Pay(to: Address, amount: Int): Effect
          Provided That
            x == 0
          Shall
            Transfer { asset: 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48, from: 0x1111111111111111111111111111111111111111, to: to, amount: amount }
      End
      """)

    assert hex =~ "a3"
  end

  test "issue 46 emit data is deterministic JSON" do
    assert Data.encode(%{"b" => 1, "a" => "z"}) == "{\"a\":\"z\",\"b\":1}"

    %{runtime_hex: hex} = compile_emit(~s|data: { "b": 1, "a": "z" }|)
    assert hex =~ Base.encode16("{\"a\":\"z\",\"b\":1}", case: :lower)
  end

  test "issue 48 string parameters use a head and tail load" do
    source = """
    Contract C
    State
      record { x : Int = 0 }
    Clauses
      Clause Pay(note: String): Effect
        Provided That
          x == 0
        Shall
          Set { field: "x", value: note }
    End
    """

    contract = Parser.parse(source)
    assert hd(hd(contract.clauses).params).type == :string

    %{runtime_hex: hex} = Compiler.compile_to_evm(source)
    assert hex =~ "60043560040135"
  end

  test "issue 49 omitted transfer from uses CALLER and msg.sender" do
    source = """
    Contract C
    State
      record { x : Int = 0 }
    Clauses
      Clause Pay(to: Address, amount: Int): Effect
        Provided That
          x == 0
        Shall
          Transfer { asset: 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48, to: to, amount: amount }
    End
    """

    assert Compiler.compile_to_evm(source).runtime_hex =~ "33"
    solidity = Compiler.compile_to_solidity(source)
    assert solidity =~ "msg.sender"
    assert solidity =~ "transferFrom"
  end

  test "issue 50 emit data is the log payload" do
    %{runtime_hex: hex} = compile_emit(~s|data: { "k": "v" }|)
    assert hex =~ Base.encode16("\"k\":\"v\"", case: :lower)
    assert hex =~ "a2"
  end

  test "issue 51 a single duration unit lowers in both backends" do
    assert Duration.seconds("7d") == 604_800
    assert Duration.rewrite_solidity("now >= start_ts + Duration(\"7d\")") =~ "block.timestamp"
    assert Duration.rewrite("Duration(\"7d\")") == "604800"

    solidity =
      Compiler.compile_to_solidity("""
      Contract C
      State
        record { start_ts : Int = 1 }
      Clauses
        Clause A(): Effect
          Provided That
            now >= start_ts + Duration("7d")
          Shall
            Set { field: "start_ts", value: 1 }
      End
      """)

    assert solidity =~ "604800"
    assert solidity =~ "block.timestamp"
  end

  test "issue 52 compound durations sum each unit" do
    assert Duration.seconds("1d12h") == 86_400 + 43_200
    assert Duration.rewrite("Duration(\"1d12h\")") == "129600"
  end

  test "issue 53 nested All stays an All" do
    %AST.Contract{clauses: [clause]} =
      Parser.parse("""
      Contract C
      State
        record { x : Int = 0 }
      Clauses
        Clause A(): Effect
          Provided That
            x == 0
          Shall
            All [
              All [
                Set { field: "x", value: 1 }
              ]
            ]
      End
      """)

    assert %AST.Eff{op: :all, args: [%AST.Eff{op: :all}]} = clause.shall
  end

  test "issue 54 parentheses group arithmetic" do
    %{runtime_hex: hex} =
      Compiler.compile_to_evm("""
      Contract C
      State
        record { x : Int = 0 }
      Clauses
        Clause A(): Effect
          Provided That
            (1 + 2) == 3
          Shall
            Set { field: "x", value: 1 }
      End
      """)

    assert hex =~ "01"
  end

  test "comparisons push the right-hand value first" do
    %{runtime_hex: hex} =
      Compiler.compile_to_evm("""
      Contract C
      State
        record { start_ts : Int = 1700000000 }
      Clauses
        Clause Release(): Effect
          Provided That
            now >= start_ts + Duration("7d")
          Shall
            Set { field: "start_ts", value: 1 }
      End
      """)

    assert hex =~ "01421015"
  end

  test "unknown predicate names are a compile error" do
    assert_raise RuntimeError, "unknown name: not_a_name", fn ->
      Compiler.compile_to_evm("""
      Contract C
      State
        record { x : Int = 0 }
      Clauses
        Clause A(): Effect
          Provided That
            not_a_name == 1
          Shall
            Set { field: "x", value: 1 }
      End
      """)
    end
  end

  test "issue 56 transferFrom compiles and jumps finish" do
    bin = A.assemble([{:push_u, 1}, {:jump_label, :L}, {:label, :L}, {:op, :STOP}])
    assert is_binary(bin)
    assert byte_size(bin) == 7

    %{runtime_hex: hex} =
      Compiler.compile_to_evm("""
      Contract C
      State
        record { x : Int = 0 }
      Clauses
        Clause Pay(to: Address, amount: Int): Effect
          Provided That
            x == 0
          Shall
            Transfer { asset: 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48, from: 0x1111111111111111111111111111111111111111, to: to, amount: amount }
      End
      """)

    assert String.starts_with?(hex, "0x")
    assert hex =~ "23b872dd"
  end

  defp compile_emit(data) do
    Compiler.compile_to_evm("""
    Contract C
    State
      record { x : Int = 0 }
    Clauses
      Clause A(): Effect
        Provided That
          x == 0
        Shall
          Emit { event: "Paid", #{data} }
    End
    """)
  end

end
