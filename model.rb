symbols = %w(1L 1 1R 1L1 1R1 b bL bR bL1 bR1 b2 b3 c cL cR cL1 cR1 c2)

tape = %w(1R 1R cL1)
cursor = 0

L = -1
R = +1

state = 1

RULES = {
  1 => {
    "1"   => ["c2", L, 1],
    "1R"  => ["1L1", R, 1],
    "1L"  => ["c2", L, 1],
    "1R1" => ["1", R, 1],
    "1L1" => ["1R1", L, 1],
    "b"   => ["bL", R, 1],
    "bR"  => ["bL1", R, 1],
    "bL"  => ["b", L, 1],
    "bR1" => ["b", R, 1],
    "bL1" => ["bR1", L, 1],
    "b2"  => ["b3", L, 2],
    "b3"  => ["bR1", L, 2],
    "c"   => ["1R", L, 2],
    "cR"  => ["cL", R, 1],
    "cL"  => ["cR1", L, 1],
    "cR1" => ["cL1", R, 2],
    "cL1" => ["H", 0, 1],
    "c2"  => ["1L", R, 1]
  },
  2 => {
    "1"   => ["1L", R, 2],
    "1R"  => ["1L", R, 2],
    "1L"  => ["1R", L, 2],
    "1R1" => ["1L1", R, 2],
    "1L1" => ["1", L, 2],
    "b"   => ["b2", R, 1],
    "bR"  => ["bL", R, 2],
    "bL"  => ["bR", L, 2],
    "bR1" => ["bL1", R, 1],
    "bL1" => ["bR1", L, 2],
    "b2"  => ["b", R, 1],
    "b3"  => ["bL1", R, 2],
    "c"   => ["cL", R, 2],
    "cR"  => ["cL", R, 2],
    "cL"  => ["cR", L, 2],
    "cR1" => ["c2", R, 2],
    "cL1" => ["c2", L, 1],
    "c2"  => ["c", L, 2]
  }
}

LABELS = {
  "1" => "1 ",
  "1R" => "1⃗ ",
  "1L" => "1⃖ ",
  "1R1" => "1⃗₁",
  "1L1" => "1⃖₁",
  "b" => "b ",
  "bR" => "b⃗ ",
  "bL" => "b⃖ ",
  "bR1" => "b⃗₁",
  "bL1" => "b⃖₁",
  "b2" => "b₂",
  "b3" => "b₃",
  "c" => "c ",
  "cR" => "c⃗ ",
  "cL" => "c⃖ ",
  "cR1" => "c⃗₁",
  "cL1" => "c⃖₁",
  "c2" => "c₂",

  "H" => "!!"
}

ENCODING = LABELS.keys.zip(('a'..'z').select {|x| x != "q" }).map(&:reverse).to_h

tape = "rrffafafaf[f]amamam"

cursor = tape.index("[")
tape = tape.chars.map {|x| ENCODING[x] }.compact
i = 0
while true
  i += 1
#   if i > 25
#     break
#   end
  puts "state: Q#{state}"
  puts "tape: %s" %
    tape.map
      .with_index {|x, i| LABELS.fetch(x) + (i == cursor - 1 ? ">" : " ") }
      .join("")
  puts

  element = tape[cursor]

  if element == "H"
    raise "won"
  else
    rule = RULES.fetch(state).fetch(element)
    tape[cursor] = rule[0]
    cursor += rule[1]
    state = rule[2]
  end

  if cursor >= tape.length
    tape.push "1L"
  elsif cursor < 0
    tape.unshift "1L"
    cursor += 1
  end
  sleep 0.01
end
