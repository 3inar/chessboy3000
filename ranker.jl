using CSV

reader = CSV.File("record.csv", header=0)

initial_rank = 800.0

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
for row in reader
  white = row[1]
  black = row[2]
  outcome = row[3]

  if !haskey(ranking, white) 
    ranking[white] = initial_rank 
  end
  if !haskey(ranking, black) 
    ranking[black] = initial_rank 
  end
  
  new_scores = elo_update(ranking[white], ranking[black], outcome)
  ranking[white] = new_scores[1]
  ranking[black] = new_scores[2]
end

ranking = sort!(collect(ranking), by=last, rev=true)

println("A218B/HDL chess federation official ranking")
println("-------------------------------------------")
for i in 1:length(ranking)
  println(string(i) * ".\t" * ranking[i][1] * "\t" * string(Int(round(ranking[i][2]))))
end


ranking[1,1]
