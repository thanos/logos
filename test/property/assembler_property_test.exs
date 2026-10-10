defmodule AssemblerPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  @moduletag :property

  property "push_u width matches the minimal unsigned encoding for uint256" do
    check all(n <- integer(0..(:erlang.bsl(1, 256) - 1)), max_runs: 30) do
      bin = Logos.EVM.Assembler.assemble([{:push_u, n}])

      expected =
        if n == 0 do
          1
        else
          1 + byte_size(:binary.encode_unsigned(n))
        end

      assert byte_size(bin) == expected
    end
  end
end
