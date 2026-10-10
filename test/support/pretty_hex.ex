defmodule TestSupport.PrettyHex do
  @moduledoc false
  def take_hex_prefix("0x" <> hex, n), do: "0x" <> String.slice(hex, 0, n * 2)
end
