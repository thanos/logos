defmodule Logos.Conformance.Watchdog do
  @moduledoc """
  Runs a function on another process and kills it when the deadline passes.

  A tight recursive loop does not read its mailbox, so the deadline is
  enforced by the caller with `Task.shutdown/2`.
  """

  @spec run((-> term()), pos_integer()) ::
          {:ok, term()} | {:error, module(), String.t()} | {:timeout, pos_integer()}
  def run(fun, timeout_ms)
      when is_function(fun, 0) and is_integer(timeout_ms) and timeout_ms > 0 do
    task =
      Task.async(fn ->
        try do
          {:ok, fun.()}
        rescue
          exception ->
            {:error, exception.__struct__, Exception.message(exception)}
        end
      end)

    case Task.yield(task, timeout_ms) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:timeout, timeout_ms}
      {:exit, _} -> {:timeout, timeout_ms}
    end
  end
end
