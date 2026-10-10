defmodule ABIOracleTest do
  use ExUnit.Case, async: true

  alias Logos.Conformance.ABI

  test "transferFrom encoding is 100 bytes with words at 4, 36, and 68" do
    encoded =
      ABI.encode_transfer_from(
        "0x1111111111111111111111111111111111111111",
        "0x2222222222222222222222222222222222222222",
        1
      )

    assert byte_size(encoded) == 100
    assert binary_part(encoded, 0, 4) == <<0x23, 0xB8, 0x72, 0xDD>>

    assert binary_part(encoded, 4, 32) ==
             ABI.word_address("0x1111111111111111111111111111111111111111")

    assert binary_part(encoded, 36, 32) ==
             ABI.word_address("0x2222222222222222222222222222222222222222")

    assert binary_part(encoded, 68, 32) == ABI.word_uint(1)
  end

  test "bool and address words reject noncanonical values" do
    assert ABI.valid_bool_word?(ABI.word_bool(true))
    assert ABI.valid_bool_word?(ABI.word_bool(false))
    refute ABI.valid_bool_word?(<<2::unsigned-big-integer-size(256)>>)
    refute ABI.valid_bool_word?(<<1>>)
    refute ABI.clean_address_word?(<<1, 0::unsigned-size(248)>>)
    refute ABI.calldata_complete?(:binary.copy(<<0>>, 99), 3)
    assert ABI.calldata_complete?(:binary.copy(<<0>>, 100), 3)
  end
end
