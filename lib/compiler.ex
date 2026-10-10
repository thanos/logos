defmodule Logos.Compiler do
  @moduledoc """

  """
  alias Logos.Parser
  alias Logos.Codegen.Solidity

  @doc "Compile DSL to Solidity.   High-level API: DSL string → Solidity source. "
  @spec compile_to_solidity(String.t()) :: String.t()
  def compile_to_solidity(dsl_text) when is_binary(dsl_text) do
    dsl_text |> Parser.parse() |> Solidity.emit()
  end

  alias Logos.Parser
  alias Logos.Codegen.EVM

  @doc """
  Compile DSL to EVM bytecode (creation & runtime)."
  High-level API: DSL string → EVM bytecode.

  Returns a map with:
    * `:runtime_hex`  — runtime code for the deployed contract
    * `:creation_hex` — creation bytecode (constructor + runtime blob)
  """
  @spec compile_to_evm(String.t()) :: %{runtime_hex: String.t(), creation_hex: String.t()}
  def compile_to_evm(dsl_text) when is_binary(dsl_text) do
    dsl_text |> Parser.parse() |> EVM.emit()
  end
end
