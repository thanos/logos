defmodule KeccakOracleTest do
  use ExUnit.Case, async: true

  alias Logos.Conformance.Keccak

  test "empty keccak and nist sha3 digests differ" do
    assert Keccak.hash_hex("") ==
             "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"

    assert Base.encode16(Keccak.sha3_256(""), case: :lower) ==
             "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"

    assert Base.encode16(:crypto.hash(:sha3_256, ""), case: :lower) ==
             Base.encode16(Keccak.sha3_256(""), case: :lower)
  end

  test "standard selectors" do
    assert Keccak.selector_hex("transfer(address,uint256)") == "a9059cbb"
    assert Keccak.selector_hex("transferFrom(address,address,uint256)") == "23b872dd"
    assert Keccak.selector_hex("balanceOf(address)") == "70a08231"
    assert Keccak.selector_hex("Release(address,uint256)") == "f6334794"
  end
end
