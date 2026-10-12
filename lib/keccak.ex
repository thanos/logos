defmodule Logos.Keccak do
  @moduledoc """
  Ethereum Keccak-256, ported from the public-domain XKCP compact permutation.

  The padding byte is `0x01`. NIST SHA3-256 uses `0x06` and is exposed only so
  tests can show the two digests differ.
  """

  import Bitwise

  @rate 136
  @mask 0xFFFFFFFFFFFFFFFF

  @spec hash(binary()) :: binary()
  def hash(data) when is_binary(data), do: sponge(data, 0x01)

  @doc "NIST SHA3-256 (domain byte 0x06). Used only to contrast with Keccak-256."
  @spec sha3_256(binary()) :: binary()
  def sha3_256(data) when is_binary(data), do: sponge(data, 0x06)

  @spec hash_hex(binary()) :: String.t()
  def hash_hex(data) when is_binary(data) do
    Base.encode16(hash(data), case: :lower)
  end

  @spec selector(binary()) :: binary()
  def selector(signature) when is_binary(signature), do: binary_part(hash(signature), 0, 4)

  @spec selector_hex(binary()) :: String.t()
  def selector_hex(signature) when is_binary(signature) do
    Base.encode16(selector(signature), case: :lower)
  end

  defp sponge(data, suffix) do
    data
    |> pad(suffix)
    |> absorb()
    |> squeeze(32)
  end

  defp pad(data, suffix) do
    pad_len = @rate - rem(byte_size(data), @rate)
    pad_len = if(pad_len == 0, do: @rate, else: pad_len)
    delim = if(pad_len == 1, do: bor(suffix, 0x80), else: suffix)
    zeros = if(pad_len > 2, do: :binary.copy(<<0>>, pad_len - 2), else: <<>>)
    tail = if(pad_len == 1, do: <<>>, else: <<0x80>>)
    data <> <<delim>> <> zeros <> tail
  end

  defp absorb(padded) do
    padded
    |> chunk(@rate)
    |> Enum.reduce(empty_lanes(), fn block, lanes ->
      lanes |> xor_block(block) |> keccak_f()
    end)
  end

  defp chunk(bin, size) do
    Stream.unfold(bin, fn
      <<>> ->
        nil

      rest ->
        <<part::binary-size(size), tail::binary>> = rest
        {part, tail}
    end)
    |> Enum.to_list()
  end

  defp empty_lanes, do: :erlang.make_tuple(25, 0)

  defp xor_block(lanes, block) do
    Enum.reduce(0..16, lanes, fn index, acc ->
      <<word::little-unsigned-64>> = binary_part(block, index * 8, 8)
      put_elem(acc, index, bxor(elem(acc, index), word))
    end)
  end

  defp squeeze(lanes, nbytes) do
    lanes
    |> Tuple.to_list()
    |> Enum.map(&<<&1::little-unsigned-64>>)
    |> IO.iodata_to_binary()
    |> binary_part(0, nbytes)
  end

  # lanes[x][y] is stored at x + 5*y, matching XKCP's byte layout.
  defp keccak_f(lanes) do
    Enum.reduce(0..23, {lanes, 1}, fn _round, {lanes, lfsr} ->
      lanes = lanes |> theta() |> rho_pi() |> chi()
      {lanes, lfsr} = iota2(lanes, lfsr)
      {lanes, lfsr}
    end)
    |> elem(0)
  end

  defp theta(lanes) do
    columns =
      for x <- 0..4 do
        Enum.reduce(0..4, 0, fn y, acc -> bxor(acc, at(lanes, x, y)) end)
      end

    for y <- 0..4, x <- 0..4 do
      d = bxor(Enum.at(columns, rem(x + 4, 5)), rotl64(Enum.at(columns, rem(x + 1, 5)), 1))
      band64(bxor(at(lanes, x, y), d))
    end
    |> write_xy()
  end

  defp rho_pi(lanes) do
    Enum.reduce(0..23, {lanes, 1, 0, at(lanes, 1, 0)}, fn t, {acc, x, y, current} ->
      x2 = y
      y2 = rem(2 * x + 3 * y, 5)
      saved = at(acc, x2, y2)
      shift = rem(div((t + 1) * (t + 2), 2), 64)
      {put_at(acc, x2, y2, rotl64(current, shift)), x2, y2, saved}
    end)
    |> elem(0)
  end

  defp chi(lanes) do
    for y <- 0..4, x <- 0..4 do
      row = for xx <- 0..4, do: at(lanes, xx, y)
      t1 = Enum.at(row, rem(x + 1, 5))
      t2 = Enum.at(row, rem(x + 2, 5))
      band64(bxor(Enum.at(row, x), band(bnot(t1), t2)))
    end
    |> write_yx()
  end

  defp iota2(lanes, lfsr) do
    {lane0, lfsr} =
      Enum.reduce(0..6, {at(lanes, 0, 0), lfsr}, fn j, {lane, r} ->
        r = rem(bxor(bsl(r, 1), bsr(r, 7) * 0x71), 256)
        lane = if(band(r, 2) != 0, do: bxor(lane, bsl(1, bsl(1, j) - 1)), else: lane)
        {band64(lane), r}
      end)

    {put_at(lanes, 0, 0, lane0), lfsr}
  end

  # for y <-, x <- produces index 5*y+x, which is x + 5*y. That matches storage.
  defp write_xy(values), do: List.to_tuple(values)

  # chi comprehension is `for y <- 0..4, x <- 0..4`, so position is 5*y+x.
  # Storage index x+5*y is the same number. write_yx is therefore write_xy.
  defp write_yx(values), do: List.to_tuple(values)

  defp at(lanes, x, y), do: elem(lanes, x + 5 * y)
  defp put_at(lanes, x, y, value), do: put_elem(lanes, x + 5 * y, value)

  defp rotl64(value, 0), do: band64(value)

  defp rotl64(value, shift) do
    value = band64(value)
    band64(bor(bsl(value, shift), bsr(value, 64 - shift)))
  end

  defp band64(value), do: band(value, @mask)
end
