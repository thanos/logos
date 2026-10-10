defmodule ConformanceCorpusTest do
  use ExUnit.Case, async: false

  alias Logos.Conformance.CorpusRunner

  for path <- Path.wildcard(Path.join(CorpusRunner.root(), "conformance/cases/**/*.json")) do
    @external_resource path
  end

  @external_resource Path.join(CorpusRunner.root(), "conformance/known_failures.json")

  @cases CorpusRunner.load_cases()
  @registry CorpusRunner.load_registry()

  setup_all do
    results = CorpusRunner.run_all(@cases)
    CorpusRunner.write_report(results)
    {:ok, by_id: Map.new(results, &{&1["id"], &1})}
  end

  for c <- @cases do
    @case_id c["id"]

    test "corpus #{@case_id}", %{by_id: by_id} do
      result = Map.fetch!(by_id, @case_id)

      assert result["status"] in CorpusRunner.allowed_statuses(),
             """
             #{@case_id} => #{result["status"]}
             #{inspect(result, pretty: true, limit: 40)}
             """
    end
  end

  test "registry covers every known_failure_id cited by a case" do
    ids = MapSet.new(@registry["failures"], & &1["id"])

    missing =
      for c <- @cases,
          id = c["known_failure_id"],
          is_binary(id),
          id not in ids,
          do: {c["id"], id}

    assert missing == []
  end
end
