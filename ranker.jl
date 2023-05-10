using CSV

reader = CSV.File("record.csv", header=0)

initial_rank = 800.0
K = 30 # https://en.wikipedia.org/wiki/Elo_rating_system

ranking = Dict()
for row in reader
  if !haskey(ranking, row[1]) 
    ranking[row[1]] = initial_rank 
  end
  if !haskey(ranking, row[2]) 
    ranking[row[2]] = initial_rank 
  end
  
  # https://en.wikipedia.org/wiki/Elo_rating_system
  r_w = ranking[row[1]]
  r_b = ranking[row[2]]

  q_w = 10^(r_w/400)
  q_b = 10^(r_b/400)

  e_w = q_w/(q_w + q_b)
  e_b = 1 - e_w

  if row[3] == "1-0"
    s_w = 1
  elseif row[3] == "0-1"
    s_w = 0
  elseif row[3] == "1/2-1/2"
    s_w = .5
  else
    error("Bad score: " * row[3])
  end

  s_b = 1 - s_w

  ranking[row[1]] = r_w + K*(s_w - e_w)
  ranking[row[2]] = r_b + K*(s_b - e_b)
end

ranking = sort!(collect(ranking), by=last, rev=true)

println("A218B/HDL chess federation official ranking")
println("-------------------------------------------")
for i in 1:length(ranking)
  println(string(i) * ".\t" * ranking[i][1] * "\t" * string(Int(round(ranking[i][2]))))
end


ranking[1,1]
