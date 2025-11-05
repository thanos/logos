defmodule Logos.Parser do
  @moduledoc false
  alias Logos.AST.{Contract, Clause, Eff}

  def parse(dsl) when is_binary(dsl) do
    lines =
      dsl
      |> String.replace("\r\n", "\n")
      |> String.split("\n")
      |> Enum.map(&String.trim_trailing/1)

    {contract, _rest} = parse_contract(lines)
    contract
  end

  # -- big picture -------------------------------------------------------------

  defp parse_contract([<<"Contract ", rest::binary>> | tail]) do
    name = rest |> String.trim()
    {sections, tail2} = collect_until(tail, "End")
    {parties, s1} = take_section(sections, "Parties", &parse_parties/1)
    {defs, s2} = take_section(s1, "Definitions", &parse_defs/1)
    {state, s3} = take_section(s2, "State", &parse_state/1)
    {clauses, s4} = take_section(s3, "Clauses", &parse_clauses/1)
    {term, s5} = take_section_optional(s4, "Term", &parse_term/1)
    {law, s6} = take_section_optional(s5, "GoverningLaw", &parse_law/1)
    {sigs, _s7} = take_section_optional(s6, "Signatures", &parse_sigs/1)

    contract = %Contract{
      name: name,
      parties: parties,
      defs: defs,
      state: state,
      clauses: clauses,
      term: term,
      law: law,
      signatures: sigs || []
    }

    {contract, tail2}
  end

  defp parse_contract(_),
    do: raise("Expected `Contract <Name>` at top of file")

  # -- section plumbing --------------------------------------------------------

  defp collect_until(lines, stop_token) do
    {pre, post} =
      Enum.split_while(lines, fn l ->
        trimmed = String.trim(l)
        trimmed != stop_token
      end)

    {pre, post}
  end

  defp take_section(lines, header, fun) do
    {block, rest} = extract_section(lines, header)
    {fun.(block), rest}
  end

  defp take_section_optional(lines, header, fun) do
    case extract_section_optional(lines, header) do
      {nil, rest} -> {nil, rest}
      {block, rest} -> {fun.(block), rest}
    end
  end

  defp extract_section(lines, header) do
    {pre, rest1} =
      Enum.split_while(lines, fn l ->
        String.trim(l) != header
      end)

    case rest1 do
      [] ->
        raise("Section `#{header}` not found")

      [_hdr | rest2] ->
        {block, rest3} = take_until_next_section(rest2)
        {block, pre ++ rest3}
    end
  end

  defp extract_section_optional(lines, header) do
    case Enum.find_index(lines, fn l -> String.trim(l) == header end) do
      nil -> {nil, lines}
      _ -> extract_section(lines, header)
    end
  end

  defp take_until_next_section(lines) do
    {block, rest} =
      Enum.split_while(lines, fn l ->
        t = String.trim(l)
        not (t == "" or section_header?(t))
      end)

    {block, rest}
  end

  defp section_header?(t) do
    t in [
      "Parties",
      "Definitions",
      "Whereas",
      "State",
      "Clauses",
      "Term",
      "GoverningLaw",
      "Signatures"
    ]
  end

  # -- sections ---------------------------------------------------------------

  defp parse_parties(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn l ->
      # id : Party = 0xAbc... or Address("...") or string
      with [id, addr] <- l |> String.split(": Party =") |> Enum.map(&String.trim/1) do
        {id, parse_value(addr)}
      else
        _ -> raise("Bad Parties line: `#{l}`")
      end
    end)
    |> Map.new()
  end

  defp parse_defs(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn l ->
      # let name : Type = expr
      case Regex.run(~r/^let\s+([a-zA-Z_]\w*)\s*:\s*([A-Za-z<>,\d_]+)\s*=\s*(.+)$/, l) do
        [_, name, _type, expr] ->
          {name, parse_value(expr)}

        _ ->
          raise("Bad Definitions line: `#{l}`")
      end
    end)
    |> Map.new()
  end

  defp parse_state(lines) do
    # Expect "record { ... }" single line block or multi-line; merge and parse fields
    blob = Enum.join(lines, " ") |> String.replace(~r/\s+/, " ")

    case Regex.run(~r/record\s*\{\s*(.+)\s*\}/, blob) do
      [_, inner] ->
        inner
        |> String.split(",")
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))
        |> Enum.map(fn f ->
          # name : Type = value
          case Regex.run(~r/^([a-zA-Z_]\w*)\s*:\s*[A-Za-z<>,\d_]+\s*=\s*(.+)$/, f) do
            [_, name, value] -> {name, parse_value(value)}
            _ -> raise("Bad State field: `#{f}`")
          end
        end)
        |> Map.new()

      _ ->
        raise("State must be `record { ... }`")
    end
  end

  defp parse_term(lines) do
    case Enum.join(lines, " ") |> String.trim() do
      <<"until ", rest::binary>> -> String.trim(rest)
      other -> raise("Term must be `until <expr>`, got: #{other}")
    end
  end

  defp parse_law(lines) do
    # GoverningLaw "text"
    txt =
      lines
      |> Enum.join(" ")
      |> String.trim()

    case Regex.run(~r/^"(.+)"$/, txt) do
      [_, s] -> s
      _ -> txt
    end
  end

  defp parse_sigs(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn l ->
      case Regex.run(~r/^require\s+([a-zA-Z_]\w*)\s+signed$/, l) do
        [_, who] -> who
        _ -> raise("Bad Signatures line: `#{l}`")
      end
    end)
  end

  defp parse_clauses(lines) do
    # Break by "Clause <Name>(...): Effect"
    chunks =
      lines
      |> chunk_by_clause_headers()

    Enum.map(chunks, &parse_clause/1)
  end

  defp chunk_by_clause_headers(lines) do
    Enum.reduce(lines, {[], []}, fn line, {acc, cur} ->
      case Regex.run(~r/^\s*Clause\s+([A-Za-z_]\w*)\s*\((.*?)\)\s*:\s*Effect\s*$/, line) do
        nil ->
          {acc, [line | cur]}

        [_m, _name, _params] ->
          # start new chunk; push previous (if any)
          acc =
            case cur do
              [] -> acc
              _ -> [Enum.reverse(cur) | acc]
            end

          {acc, [line]}
      end
    end)
    |> then(fn {acc, cur} ->
      acc =
        case cur do
          [] -> acc
          _ -> [Enum.reverse(cur) | acc]
        end

      Enum.reverse(acc)
    end)
  end

  defp parse_clause(block_lines) do
    [hdr | tail] = block_lines

    [_, name, params_raw] =
      Regex.run(~r/^\s*Clause\s+([A-Za-z_]\w*)\s*\((.*?)\)\s*:\s*Effect\s*$/, hdr)

    params =
      params_raw
      |> String.trim()
      |> case do
        "" ->
          []

        s ->
          s
          |> String.split(",")
          |> Enum.map(&String.trim/1)
          |> Enum.map(fn p ->
            # allow "x: Type" or just "x"
            case String.split(p, ":") |> Enum.map(&String.trim/1) do
              [id, _t] -> id
              [id] -> id
            end
          end)
      end

    {prov_block, after_prov} = take_after_marker(tail, "Provided That")
    {shall_block, maybe_other} = take_after_marker(after_prov, "Shall")

    {remedies, _rest} =
      case take_after_marker_optional(maybe_other, "Otherwise") do
        {nil, r1} ->
          {nil, r1}

        {_o, r1} ->
          {rem, r2} = take_after_marker(r1, "Remedies")
          {parse_effect(rem), r2}
      end

    provided =
      prov_block
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))

    shall = parse_effect(shall_block)

    %Clause{name: name, params: params, provided: provided, shall: shall, remedies: remedies}
  end

  defp take_after_marker(lines, marker) do
    idx =
      Enum.find_index(lines, fn l -> String.trim(l) == marker end) ||
        raise("Expected `#{marker}` block")

    pre = Enum.drop(lines, idx + 1)
    # take until next header word of same clause or section
    {block, rest} =
      Enum.split_while(pre, fn l ->
        t = String.trim(l)

        not (t in ["Shall", "Otherwise", "Remedies"] or Regex.match?(~r/^\s*Clause\s+/, t) or
               section_header?(t))
      end)

    {block, rest}
  end

  defp take_after_marker_optional(lines, marker) do
    case Enum.find_index(lines, fn l -> String.trim(l) == marker end) do
      nil ->
        {nil, lines}

      idx ->
        pre = Enum.drop(lines, idx + 1)

        {block, rest} =
          Enum.split_while(pre, fn l ->
            t = String.trim(l)

            not (t in ["Shall", "Otherwise", "Remedies"] or Regex.match?(~r/^\s*Clause\s+/, t) or
                   section_header?(t))
          end)

        {block, rest}
    end
  end

  # -- effect parsing (small, compositional) -----------------------------------

  defp parse_effect(lines) do
    text =
      lines
      |> Enum.join(" ")
      |> String.replace(~r/\s+/, " ")
      |> String.trim()

    cond do
      text == "" ->
        %Eff{op: :noop, args: %{}}

      String.starts_with?(text, "All [") ->
        inner = text |> strip_prefix("All [") |> strip_suffix("]")
        parts = split_top_level(inner, ",")
        effects = Enum.map(parts, &parse_effect_term/1)
        %Eff{op: :all, args: effects}

      true ->
        parse_effect_term(text)
    end
  end

  defp parse_effect_term(text) do
    cond do
      String.starts_with?(text, "Set {") ->
        kv = parse_kv_map(text, "Set")

        %Logos.AST.Eff{
          op: :set,
          args: %{
            "field" => must(kv, "field"),
            "value" => parse_value(must(kv, "value"))
          }
        }

      String.starts_with?(text, "Emit {") ->
        kv = parse_kv_map(text, "Emit")

        %Logos.AST.Eff{
          op: :emit,
          args: %{
            "event" => must(kv, "event") |> strip_quotes(),
            "data" => Map.get(kv, "data", "%{}")
          }
        }

      String.starts_with?(text, "If {") ->
        kv = parse_kv_map(text, "If")

        %Logos.AST.Eff{
          op: :if,
          args: %{
            "cond" => must(kv, "cond"),
            "then" => parse_effect(maybe_wrap_list(must(kv, "then_"))),
            "else" => parse_effect(maybe_wrap_list(must(kv, "else_")))
          }
        }

      String.starts_with?(text, "Transfer {") ->
        kv = parse_kv_map(text, "Transfer")
        %Logos.AST.Eff{op: :transfer, args: kv}

      true ->
        # Try to treat it as a single effect inside "All [ ... ]" omission
        %Logos.AST.Eff{op: :emit, args: %{"event" => "Note", "data" => %{"raw" => text}}}
    end
  end

  defp parse_kv_map(text, head) do
    body =
      text
      |> strip_prefix("#{head} {")
      |> strip_suffix("}")
      |> String.trim()

    split_top_level(body, ",")
    |> Enum.map(fn pair ->
      case String.split(pair, ":", parts: 2) do
        [k, v] -> {String.trim(k), String.trim(v)}
        _ -> raise("Bad KV in #{head}: `#{pair}`")
      end
    end)
    |> Map.new()
  end

  defp must(map, key), do: Map.fetch!(map, key)

  defp maybe_wrap_list(s) do
    st = String.trim(s)

    if String.starts_with?(st, "All [") do
      st
    else
      "All [ " <> st <> " ]"
    end
  end

  # -- tiny expr/value parser --------------------------------------------------

  defp parse_value(v) do
    v = String.trim(v)

    cond do
      v in ["true", "false"] ->
        v == "true"

      String.match?(v, ~r/^-?\d+$/) ->
        String.to_integer(v)

      String.match?(v, ~r/^0x[0-9a-fA-F]{4,}$/) ->
        v

      String.match?(v, ~r/^"([^"]*)"$/) ->
        strip_quotes(v)

      String.match?(v, ~r/^Some\((.+)\)$/) ->
        {:some, parse_value(Regex.replace(~r/^Some\(/, v, "") |> strip_suffix(")"))}

      v == "None" ->
        :none

      true ->
        # leave as raw expr id or call; codegen will map where possible
        {:raw, v}
    end
  end

  defp strip_prefix(s, p) do
    if String.starts_with?(s, p), do: String.replace_prefix(s, p, ""), else: s
  end

  defp strip_suffix(s, suf) do
    if String.ends_with?(s, suf), do: String.replace_suffix(s, suf, ""), else: s
  end

  defp strip_quotes(s) do
    s |> String.trim() |> String.trim_leading("\"") |> String.trim_trailing("\"")
  end

  # Split by delimiter at top-level (not inside {...} / [...] / quotes)
  defp split_top_level(s, delim) do
    {parts, buf, _lvl, _in_q} =
      String.codepoints(s)
      |> Enum.reduce({[], "", 0, false}, fn ch, {acc, cur, lvl, q} ->
        cond do
          ch == "\"" ->
            {acc, cur <> ch, lvl, !q}

          q ->
            {acc, cur <> ch, lvl, q}

          ch == "{" or ch == "[" ->
            {acc, cur <> ch, lvl + 1, q}

          ch == "}" or ch == "]" ->
            {acc, cur <> ch, lvl - 1, q}

          ch == String.first(delim) and lvl == 0 and
              String.ends_with?(cur, String.slice(delim, 0, 0)) ->
            # single-char delim assumed
            {[String.trim(cur) | acc], "", lvl, q}

          ch == "," and lvl == 0 and delim == "," ->
            {[String.trim(cur) | acc], "", lvl, q}

          true ->
            {acc, cur <> ch, lvl, q}
        end
      end)

    parts = [buf | parts] |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == ""))
    Enum.reverse(parts)
  end
end
