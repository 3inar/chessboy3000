using CSV

reader = CSV.File("record.csv", header=0)

initial_score = 800.0

# https://en.wikipedia.org/wiki/Elo_rating_system
function elo_update(white_rank, black_rank, outcome, K=30)
  r_w = white_rank
  r_b = black_rank

  q_w = 10^(r_w/400)
  q_b = 10^(r_b/400)

  e_w = q_w/(q_w + q_b)
  e_b = 1 - e_w

  if outcome == "1-0"
    s_w = 1
  elseif outcome == "0-1"
    s_w = 0
  elseif outcome == "1/2-1/2"
    s_w = .5
  else
    error("Invalid outcome: " * outcome)
  end

  s_b = 1 - s_w

  return [r_w + K*(s_w - e_w), r_b + K*(s_b - e_b)]
end

ranking = Dict()
activity = Dict()

counter = 1
for row in reader
  white = row[1]
  black = row[2]
  outcome = row[3]

  activity[white] = counter
  activity[black] = counter

  if !haskey(ranking, white) 
    ranking[white] = initial_score
  end
  if !haskey(ranking, black) 
    ranking[black] = initial_score
  end
  
  new_scores = elo_update(ranking[white], ranking[black], outcome)
  ranking[white] = new_scores[1]
  ranking[black] = new_scores[2]

  global counter = counter + 1
end

# removes inactive players 
inactive_lim = maximum(values(activity)) - 50
for (key, value) in activity
    if value < inactive_lim
      delete!(ranking, key)
    end
end

longestname = maximum(length.(keys(ranking)))

ranking = sort!(collect(ranking), by=last, rev=true)
padto = 4*Int(ceil((longestname + 1)/4.0))

println("A218B/HDL chess federation official ranking")
println("-------------------------------------------")
for i in 1:length(ranking)
  if i < 10
    pad1 = "  "
  else
    pad1 = " "
  end

  pad2 = repeat(" ", padto - length(ranking[i][1]))

  println(string(i) * "." * pad1 * 
          ranking[i][1] * pad2 * 
          string(Int(round(ranking[i][2]))))
end

