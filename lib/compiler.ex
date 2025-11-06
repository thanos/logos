defmodule Logos.Compiler do
  @moduledoc """
  Public entrypoint. Takes DSL string -> Solidity source string.
  """
  alias Logos.Parser
  alias Logos.Codegen.Solidity

  def compile_to_solidity(dsl_text) when is_binary(dsl_text) do
    dsl_text
    |> Parser.parse()
    |> Solidity.emit()
  end

  alias Logos.Parser
  alias Logos.Codegen.EVM

  def compile_to_evm(dsl_text) when is_binary(dsl_text) do
    dsl_text
    |> Parser.parse()
    |> EVM.emit()
  end
end
