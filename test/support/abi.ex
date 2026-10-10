defmodule Logos.Conformance.ABI do
  @moduledoc """
  Independent ABI word encoder used as an oracle. It does not call the Logos compiler.
  """

  alias Logos.Conformance.Keccak

  @uint_max :erlang.bsl(1, 256)

  def word_uint(value) when is_integer(value) and value >= 0 and value < @uint_max do
    <<value::unsigned-big-integer-size(256)>>
  end

  def word_address(<<"0x", hex::binary>>) do
    {:ok, bytes} = Base.decode16(hex, case: :mixed)
    true = byte_size(bytes) == 20
    <<0::unsigned-big-integer-size(96), bytes::binary>>
  end

  def word_bool(true), do: word_uint(1)
  def word_bool(false), do: word_uint(0)

  def selector(signature), do: Keccak.selector(signature)

  @doc """
  Head-only encoding: 4-byte selector followed by static 32-byte words.
  """
  def encode_call(signature, words) when is_binary(signature) and is_list(words) do
    selector(signature) <> IO.iodata_to_binary(words)
  end

  def encode_transfer_from(from, to, amount) do
    encode_call("transferFrom(address,address,uint256)", [
      word_address(from),
      word_address(to),
      word_uint(amount)
    ])
  end

  def valid_bool_word?(<<0::unsigned-big-integer-size(256)>>), do: true
  def valid_bool_word?(<<1::unsigned-big-integer-size(256)>>), do: true
  def valid_bool_word?(<<_::binary-size(32)>>), do: false
  def valid_bool_word?(_), do: false

  def clean_address_word?(<<0::unsigned-big-integer-size(96), _::binary-size(20)>>), do: true
  def clean_address_word?(<<_::binary-size(32)>>), do: false
  def clean_address_word?(_), do: false

  def calldata_complete?(calldata, arg_count)
      when is_binary(calldata) and is_integer(arg_count) do
    byte_size(calldata) >= 4 + 32 * arg_count
  end
end
