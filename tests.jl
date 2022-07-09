include("main.jl")

using Test

@testset "jahtzeebot" begin

    @testset "known_values" begin 
        # this should be 20.73 per http://www-set.win.tue.nl/~wstomv/misc/yahtzee/osyp.php
        game = GameState( 
            rolls_remaining= 2,
            sorted_dievals= DieVals(3,4,4,6,6),
            sorted_open_slots= SortedSlots([6,12]), 
        )
        app = App(game)
        build_cache!(app)
        lhs=app.ev_cache[game]
        @test lhs.ev == 20.73   atol=2
    end

end;