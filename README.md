# Compound Poisson Process with Exponential Jumps (Shiny App)

This project simulates and visualizes the Compound Poisson process:

S(t) = ∑ Xᵢ where N(t) ~ Poisson(λt) and Xᵢ ~ Exponential(β)

---

## Live Shiny App
https://compoundpoisson.shinyapps.io/documents/

---

##  Theoretical Summary

- If N(t) = n, then S(t) ~ Gamma(n, β)
- P(S(t) = 0) = exp(-λt)
- Mean: E[S(t)] = λt / β
- Variance: Var[S(t)] = 2λt / β²

Parameter effects:
- ↑ λ → more frequent jumps → S(t) grows faster
- ↓ β → larger jump sizes → distribution shifts right

---

## Run Locally

Install required packages:
```r
install.packages("shiny")
install.packages("rsconnect")

# Run the app:
shiny::runApp()

Author
Utsha Ghosh
