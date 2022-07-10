include("../src/julzeebot.jl")

using Test

@testset "jahtzeebot" begin

    @testset "count_of_n_choose_x_items" begin 
        @test n_take_r(6,3,order_matters=true,with_replacement=true)== 216 
        @test n_take_r(6,3,order_matters=true,with_replacement=false)==120
        @test n_take_r(6,3,order_matters=false,with_replacement=true)==56
        @test n_take_r(6,3,order_matters=false,with_replacement=false)== 20
    end

    @testset "score_aces" begin 
        @test score_aces((2,1,2,1,5,)) == 2
        @test score_aces((2,3,2,6,5,)) == 0 
    end

    @testset "score_upper_box" begin
        @test score_upperbox(4,(1,2,3,4,5,)) == 4
        @test score_upperbox(4,(4,4,4,4,4,)) == 20
        @test score_upperbox(4,(1,2,3,6,5,)) == 0 
    end

    @testset "score_three_of_a_kind" begin
        @test score_three_of_a_kind(sort([5,1,2,5,5]))==18
        @test score_three_of_a_kind(sort([3,2,2,3,1]))==0
        @test score_three_of_a_kind(sort([6,6,1,6,6]))==25
    end

    @testset "score_four_of_a_kind" begin
        @test score_four_of_a_kind(sort([3,2,3,3,3]))==14
        @test score_four_of_a_kind(sort([3,2,2,3,3]))==0
        @test score_four_of_a_kind(sort([3,3,3,3,3]))==15
    end 

    @testset "score_fullhouse" begin
        @test score_fullhouse((2,2,3,3,3,)) == 25
        @test score_fullhouse((2,2,3,3,2,)) == 25
        @test score_fullhouse((2,2,1,3,3,)) == 0 
        @test score_fullhouse((2,3,3,3,3,)) == 0 
        @test score_fullhouse((1,2,3,4,5,)) == 0 
        @test score_fullhouse((3,3,3,3,3,)) == 0 
    end

    @testset "score_sm_straight" begin
        @test score_sm_str8(sort([1,3,2,4,6])) == 30
        @test score_sm_str8(sort([1,3,2,4,5])) == 30
        @test score_sm_str8(sort([1,3,2,6,5])) == 0 
    end

    @testset "score_lg_straight" begin
        @test score_lg_str8(sort([1,3,2,4,6])) == 0
        @test score_lg_str8(sort([1,3,2,4,5])) == 40
        @test score_lg_str8(sort([1,3,2,6,5])) == 0 
    end

    @testset "score_yahtzee" begin
        @test score_yahtzee(sort([2,2,2,2,2])) == 50 
        @test score_yahtzee(sort([2,2,6,2,2])) == 0 
    end

    @test straight_len(sort([1,2,0,5,3])) == 4

    @test_skip @test "ev_of_yahtzee_in_1_roll" begin
    # see https://www.yahtzeemanifesto.com/yahtzee-odds.php 
        game = GameState(   rolls_remaining= 1, 
                            sorted_open_slots= [YAHTZEE], 
                            sorted_dievals= [1,2,3,4,5],
        )
        app = App(game)
        build_cache!(app)
        result = app.ev_cache[app.game]
        in_1_odds = 6.0/7776.0; 
        @test result.ev == in_1_odds * 50.0    atol=0.1 
    end

    # @test_skip @testset "known_values" begin 
    #     # this should be 20.73 per http://www-set.win.tue.nl/~wstomv/misc/yahtzee/osyp.php
    #     game = GameState( 
    #         rolls_remaining= 2,
    #         sorted_dievals= DieVals(3,4,4,6,6),
    #         sorted_open_slots= SortedSlots([6,12]), 
    #     )
    #     app = App(game)
    #     build_cache!(app)
    #     lhs=app.ev_cache[game]
    #     @test lhs.ev == 20.73   atol=2
    # end

end