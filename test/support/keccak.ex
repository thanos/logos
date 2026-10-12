defmodule Logos.Conformance.Keccak do
  @moduledoc """
  Test alias for `Logos.Keccak`. The compiler and the oracle share one sponge.
  """

  defdelegate hash(data), to: Logos.Keccak
  defdelegate sha3_256(data), to: Logos.Keccak
  defdelegate hash_hex(data), to: Logos.Keccak
  defdelegate selector(signature), to: Logos.Keccak
  defdelegate selector_hex(signature), to: Logos.Keccak
end
