# A Yahtzee bot written in Julia 

Mostly this was an exercise in learning and evaluating Julia as an alternative to Rust and Python. 

It rolls virtual dice several hundred trillion times to calculate the optimal player choice for every possible scenario in a game of solo Yahtzee.

This runs in 203 seconds on a 2021 Macbook Pro. 

This compares favorably to ~10 minutes for the [implementation written in Rust](
https://github.com/mode80/yahtzeebot), mostly due to availibility of better tooling for performance profiling, and convenient language features for multithreading and SIMD performance boosts.

(A comparable implementation in Python was on track to finish in 50 DAYS.)

At present it simply returns the expected final score for a solo game of Yahtzee at the point of the first roll with an empty scorecard. 

The main() function can be readily tweaked to return the expected final score and best course of action for any valid game state.