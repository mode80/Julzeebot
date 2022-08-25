# module Julzeebot
# import StaticArrays: SVector
import Combinatorics: permutations, with_replacement_combinations, combinations, powerset
import DataStructures: counter
import Base.Iterators
using Base.Threads
# import InteractiveUtils
using Base
using ProgressMeter 
# using Infiltrator
using OffsetArrays
# using .Threads 

# # ExportAll https://discourse.julialang.org/t/exportall/4970/2
# for n in names(@__MODULE__; all=true)
#     if Base.isidentifier(n) && n ∉ (Symbol(@__MODULE__), :eval, :include)
#         @eval export $n
#     end
# end

#=-------------------------------------------------------------
CONSTS, UTILS
-------------------------------------------------------------=#
const u8=UInt8; 
const u16=UInt16; 
const u32=UInt32; 
const f32=Float32; 
const f64=Float64; # lazy Rust-like abbreviations

const Selection = u8 # a bitfield representing a selection of dice to roll (1 means roll, 0 means don't)
const Choice = u8 # represents EITHER chosen scorecard Slot, OR a chosen dice Selection (below)
const DieVal = u8 # a single die value 0 to 6 where 0 means "unselected"
const Slot = u8
const GameStateID = u32

# a single scorecard slot with values ranging from ACES to CHANCE 
const ACES = 0x1; const TWOS = 0x2; const THREES = 0x3; const FOURS = 0x4; const FIVES = 0x5; const SIXES = 0x6;
const SM_STRAIGHT = 0xA; const LG_STRAIGHT = 0xB; const YAHTZEE = 0xC; const CHANCE = 0xD;
const THREE_OF_A_KIND = 0x7; const FOUR_OF_A_KIND = 0x8; const FULL_HOUSE = 0x9; 

#=-------------------------------------------------------------
ChoiceEV
-------------------------------------------------------------=#
struct ChoiceEV 
    choice::Choice
    ev::f32
end


#=-------------------------------------------------------------
DieVals
-------------------------------------------------------------=#
struct DieVals <: AbstractArray{DieVal, 1} #TODO make immutable to live on the stack in Julia
    data::u16 # 5 dievals, each from 0 to 6, can be encoded in 2 bytes total, each taking 3 bits
end

DieVals() = DieVals(0) 

DieVals(d1::T, d2::T, d3::T, d4::T, d5::T) where {T<:Number} = let 
    DieVals(u16(d5) << 12 | u16(d4) << 9 | u16(d3) << 6 | u16(d2) << 3 | u16(d1))
end

# blit the 'from' dievals into the 'self' dievals with the help of a mask where 0 indicates incoming 'from' bits and 1 indicates none incoming 
blit(self::DieVals, from::DieVals, mask::DieVals,) = 
    DieVals((self.data & mask.data) | from.data)

# Base.convert(::Type{DieVals}, from::Vector{Int64}) = DieVals(from) 

Base.copy(self::DieVals) = DieVals(self.data)

Base.IndexStyle(::Type{<:DieVals}) = IndexLinear()

Base.size(_::DieVals) = return (5,) 

Base.length(_::DieVals) = return 5 

Base.getindex(self::DieVals, i) ::DieVal = ((self.data >> ((i-1)*3)) & 0b111) 

setindex(self::DieVals, val::DieVal, i) = let #TODO removable for immutable DieVals? 
    bitpos = 3*(i-1) # widths of 3 bits per value
    mask = ~(UInt16(0b111) << bitpos) # hole maker
    DieVals( (self.data&mask) | (UInt16(val)<<bitpos) )#  #  punch & fill hole
end

Base.isequal(self::DieVals, other::DieVals) = isequal(self.data, other.data) 

Base.hash(self::DieVals, h::UInt) = hash(self.data,h)


#=-------------------------------------------------------------
SortedSlots
-------------------------------------------------------------=#
struct Slots <: AbstractArray{Slot, 1} 
    data::u16 # 13 sorted Slots can be positionally encoded in one u16
end

Slots(args::Slot...) = let  #be careful about dispatching to the wrong constructor for single slots (must use slot of type Slot)
    data::u16 = 0
    for slot in args 
        mask = 0x0001 << slot
        data |= mask # force on
    end
    Slots(data) 
end

Base.hash(self::Slots, h::UInt) = hash(self.data,h)

Base.isequal(self::Slots, other::Slots) = isequal(self.data, other.data)

Base.iterate(self::Slots, state=self.data) = let  #TODO it'd be faster to keep peeled slots.data in state rather than the index 
    if state == 0x0000 return nothing end 
    zero_count = trailing_zeros(state) 
    mask = ~( 0x001 << u16(zero_count) )
    newstate::u16 = state & mask # force off
    return ( Slot(zero_count), newstate) 
end

Base.eltype(::Type{Slots}) = Slot 

Base.length(self::Slots) = count_ones(self.data) 

Base.size(self::Slots) = (count_ones(self.data),)

Base.copy(self::Slots) = Slots(self.data)

Base.getindex(self::Slots, i)::Slot= let
    # @assert(i<=length(self))
    bits = self.data
    bit_index=0
    for _ in 1:i  # the slots.data does not use the rightmost (0th) bit; this is for more performant interactions with 1-based array
        bit_index = trailing_zeros(bits)
        bits &= ~( 1 << u16(bit_index) )  #unset bit
    end
    return Slot(bit_index)
end

has(slots::Slots, slot) ::Bool = let
    0x0000 < slots.data & (0x0001<<u16(slot))  
end

# insert!(self::SortedSlots, slots... ) = let
#     for slot in slots
#         mask = 1 << u16(slot)
#         self.data |= mask # force on
#     end
# end

remove(self::Slots, slot_to_remove ) ::Slots = let
    mask = ~( 1 << u16(slot_to_remove) )
    newdata::u16 = self.data & mask # force off
    return Slots(newdata)
end

used_upper_slots(unused_slots::Slots) ::Slots = let
    all_bits_except_unused_uppers = ~unused_slots.data # "unused" slots (as encoded in .data) are not "previously used", so blank those out
    all_upper_slot_bits = u16((1<<7)-2)  # upper slot bits are those from 2^1 through 2^6 (.data encoding doesn't use 2^0)
    previously_used_upper_slot_bits = all_bits_except_unused_uppers & all_upper_slot_bits
    return Slots( previously_used_upper_slot_bits )
end

# these are all the possible score entries for each upper slot
const UPPER_SCORES = ( 
    [0,1,2,3,4,5],      # ACES
    [0,2,4,6,8,10],     # TWOS
    [0,3,6,9,12,15],    # THREES 
    [0,4,8,12,16,20],   # FOURS
    [0,5,10,15,20,25],  # FIVES
    [0,6,12,18,24,30],  # SIXES
)

""" returns the unique and relevant "upper bonus total" that could have occurred from the previously used upper slots """
relevant_upper_totals(slots::Slots) :: Vector{u8} = let # TODO switch to this simplified version in the Rust implmentation for fairness?
    totals = Set(u8[])
    used_slots = used_upper_slots(slots)
    slots_val_sets = [UPPER_SCORES[i] for i in used_slots] 
    used_score_perms = Iterators.product(slots_val_sets...)
    for perm in used_score_perms # TODO this is a wasteful approach that isn't much impact on pref but allocates unneeded GBs 
        tot = sum( perm; init=0 )
        push!(totals, min(tot,63) )
    end 
    push!(totals,0) # 0 is always relevant and must be added here explicitly when there are no used upper slots 

    # filter out the lower totals that aren't relevant because they can't reach the goal with the upper slots remaining 
    # this filters out a lot of dead state space but means the lookup function must later map extraneous deficits to a default 
    best_current_slot_total = best_upper_total(slots)
    [x for x in totals if x==0 || x + best_current_slot_total >=63]
end

# a non-exact but fast estimate of relevant_upper_totals
useful_upper_totals(all_unused_slots::Slots) = let 
    totals = (x for x in 0:63)
    used_uppers = used_upper_slots(all_unused_slots)
    if all(iseven,used_uppers) 
        totals = (x for x in totals if iseven(x)) 
    end
    # filter out the lowish totals that aren't relevant because they can't reach the goal with the upper slots remaining 
    # this filters out a lot of dead state space but means the lookup function must later map extraneous deficits to a default 
    best_unused_slot_total = best_upper_total(all_unused_slots)
    totals = (x for x in totals if x + best_unused_slot_total >=63 || x==0)
    return totals
end

best_upper_total(self::Slots) ::u8 = let
    sum=0
    for x in self  
        if 6<x break end
        sum+=x
    end
    sum*5
end


#=-------------------------------------------------------------
Outcome
-------------------------------------------------------------=#
struct Outcome  
    dievals::DieVals
    mask::DieVals # stores a pre-made mask for blitting this outcome onto a GameState.DieVals.data u16 later
    arrangements::f32  # how many indistinguisable ways can these dievals be arranged (ie swapping identical dievals)
end

#=-------------------------------------------------------------
GameState
-------------------------------------------------------------=#

struct GameState # TODO test impact of calling keyword funcs are bad for performance https://techytok.com/code-optimisation-in-julia/#keyword-arguments 
    id :: GameStateID # 30 bits # with the id, 
    #we can store all of below in a sparse array using 2^(8+13+6+2+1) entries = 1_073_741_824 entries = 5.2GB when storing 40bit ResultEVs 
    sorted_dievals ::DieVals # 15 bits OR 8 bits once convereted to a DieValID (252 possibilities)
    open_slots ::Slots # 13 bits        = 8_192 possibilities
    upper_total ::u8 # = 6 bits         = 64    "
    rolls_remaining ::u8 # = 2 bits     = 4     "  
    yahtzee_bonus_avail ::Bool # = 1bit = 2     "
    GameState(sorted_dievals, open_slots, upper_total, rolls_remaining, yahtzee_bonus_avail) = let 
        @inbounds dievals_id::u32 = SORTED_DIEVALS[sorted_dievals.data].id # this is the 8-bit encoding of self.sorted_dievals
        id= dievals_id |                 # self.id will use 30 bits total...
            (u32(open_slots.data)    << 7)  | # slots.data doesn't use its rightmost bit so we only shift 7 to make room for the 8-bit dieval_id above 
            (u32(upper_total)        << 21) | # make room for 13+8 bit stuff above 
            (u32(rolls_remaining)    << 27) | # make room for the 13+8+6 bit stuff above
            (u32(yahtzee_bonus_avail)<< 29)   # make room for the 13+8+6+2 bit stuff above
        new(id, sorted_dievals, open_slots, upper_total, rolls_remaining, yahtzee_bonus_avail) 
    end
end


# combines all GameState field bits to form a unique GameState ID
Base.hash(self::GameState, h::UInt) = 
    hash(
        self.sorted_dievals.data, hash(
            self.open_slots.data, hash(
                self.upper_total, hash(
                    self.rolls_remaining, hash(
                        self.yahtzee_bonus_avail, h
    )))))

Base.isequal(self::GameState, other::GameState) = 
    isequal(self.sorted_dievals.data, other.sorted_dievals.data) && 
    isequal(self.open_slots.data, other.open_slots.data) && 
    isequal(self.upper_total, other.upper_total) && 
    isequal(self.rolls_remaining, other.rolls_remaining) && 
    isequal(self.yahtzee_bonus_avail, other.yahtzee_bonus_avail) 

 
# calculate relevant counts for gamestate: required lookups and saves
counts(self::GameState) :: Tuple{Int,Int} = let 
    lookups = 0 
    saves = 0 
    false_true = (true, false); just_false = (false,)
    for subset_len in 1:length(self.open_slots)
        for slots_vec in combinations( collect(self.open_slots), subset_len )  
            slots = Slots(slots_vec...)
            joker_rules = has(slots,YAHTZEE) # yahtzees aren't wild whenever yahtzee slot is still available 
            totals = useful_upper_totals(slots) 
            for _ in totals 
                for __ in ifelse(joker_rules, false_true, just_false ) 
                    slot_lookups = (subset_len * ifelse(subset_len==1, 1, 2) ) * 252 #// * subset_len as u64;
                    dice_lookups = 848484 # // previoiusly verified by counting up by 1s in the actual loop. however chunking forward is faster 
                    lookups += dice_lookups + slot_lookups
                    saves+=1
    end end end end 
    return ( lookups, saves ) 
end 
 
score_first_slot_in_context(self::GameState) ::u8 = let

    # score slot itself w/o regard to game state 
        slot::Slot, _ = iterate(self.open_slots)
        score = score_slot_with_dice(slot, self.sorted_dievals) 

    # add upper bonus when needed total is reached 
        if slot<=SIXES && self.upper_total<0x3f #63
            new_total = min(self.upper_total+score, 0x3f #=63=#) 
            if new_total==0x3f #63 # we just reach bonus threshold
                score += 0x23 #35  # add the 35 bonus points 
            end
        end  

    # special handling of "joker rules" */
        just_rolled_yahtzee = score_yahtzee(self.sorted_dievals)==50
        joker_rules_in_play = (slot != YAHTZEE) # joker rules in effect when the yahtzee slot is not open 
        if just_rolled_yahtzee && joker_rules_in_play # standard scoring applies against the yahtzee dice except ... 
            if slot==FULL_HOUSE  score=25 end
            if slot==SM_STRAIGHT score=30 end
            if slot==LG_STRAIGHT score=40 end
        end

    # # special handling of "extra yahtzee" bonus per rules*/
        if just_rolled_yahtzee && self.yahtzee_bonus_avail score+=100 end 

    return score
end 

print_state_choices_header() = let
    println("choice_type,choice,dice,rolls_remaining,upper_total,yahtzee_bonus_avail,open_slots,expected_value");
end 

print_state_choice(state ::GameState, choice_ev ::ChoiceEV) = let
    if state.rolls_remaining==0 
        # println!("S,{},{},{},{},{},{},{}",
        println("S, $(choice_ev.choice), $(state.sorted_dievals), $(state.rolls_remaining), $(state.upper_total), $(state.yahtzee_bonus_avail ? "Y" : ""), $(state.open_slots), $(choice_ev.ev)") 
    else 
        println("S, $(choice_ev.choice), $(state.sorted_dievals), $(state.rolls_remaining), $(state.upper_total), $(state.yahtzee_bonus_avail ? "Y" : ""), $(state.open_slots), $(choice_ev.ev)") 
        # println!("D,{:05b},{},{},{},{},{},{}",
        #     choice_ev.choice, state.sorted_dievals, state.rolls_remaining, state.upper_total, 
        #     state.yahtzee_bonus_avail ? "Y" : "", state.sorted_open_slots, choice_ev.ev) 
    end 
end 


#=-------------------------------------------------------------
SCORING FNs
-------------------------------------------------------------=#

score_upperbox(boxnum, sorted_dievals) ::u8 = let #TODO rename SortedDieVals to DieValsID to avoid ambiguitiy in signatures like this one 
    sum::u8 = 0
    for d in sorted_dievals
        if d==boxnum sum+=boxnum end
    end
    return sum 
end 

score_n_of_a_kind(n, sorted_dievals) ::u8 = let 
    inarow=1; maxinarow=1; lastval=100; sum=0; 
    for x in sorted_dievals 
        if (x==lastval && x!=0) inarow +=1 else inarow=1 end
        maxinarow = max(inarow,maxinarow)
        lastval = x
        sum+=x
    end 
    if (maxinarow>=n) return sum else return 0 end
end 

straight_len(sorted_dievals) ::u8 = let
    inarow=1 
    lastval=254 # stub
    maxinarow=1
    for x in sorted_dievals 
        if (x==lastval+1 && x!=0) 
            inarow+=1 
        elseif x!=lastval 
            inarow=1 
        end
        maxinarow = max(inarow,maxinarow)
        lastval = x
    end  
    maxinarow 
end

score_aces(sorted_dievals)      ::u8         = score_upperbox(0x1,sorted_dievals)   
score_twos(sorted_dievals)      ::u8         = score_upperbox(0x2,sorted_dievals) 
score_threes(sorted_dievals)    ::u8         = score_upperbox(0x3,sorted_dievals) 
score_fours(sorted_dievals)     ::u8         = score_upperbox(0x4,sorted_dievals) 
score_fives(sorted_dievals)     ::u8         = score_upperbox(0x5,sorted_dievals) 
score_sixes(sorted_dievals)     ::u8         = score_upperbox(0x6,sorted_dievals) 
    
score_three_of_a_kind(sorted_dievals)   ::u8     = score_n_of_a_kind(0x3,sorted_dievals) 
score_four_of_a_kind(sorted_dievals)    ::u8     = score_n_of_a_kind(0x4,sorted_dievals) 
score_sm_str8(sorted_dievals)           ::u8     = ifelse( straight_len(sorted_dievals) >= 0x4, 30, 0)
score_lg_str8(sorted_dievals)           ::u8     = ifelse( straight_len(sorted_dievals) == 0x5, 40, 0)

# The official rule is that a Full House is "three of one number and two of another
score_fullhouse(sorted_dievals) ::u8 = let
   
    valcounts1 = valcounts2 = 0
    j=0

    for (i,val) in enumerate(sorted_dievals) 
        if val==0 return 0 end
        if (j==0 || sorted_dievals[i]!=sorted_dievals[i-1]) 
            j+=1 
        end
        if j==1 valcounts1+=1 end
        if j==2 valcounts2+=1 end
        if j==3 return 0 end
    end

    if valcounts1==3 && valcounts2==2 || valcounts2==3 && valcounts1==2 return 25 end
    return 0 

end 
    
score_chance(sorted_dievals) ::u8 = sum(sorted_dievals) 
    
score_yahtzee(sorted_dievals) ::u8 =
    (sorted_dievals[1] == sorted_dievals[5] != 0) ? 50 : 0 

# reports the score for a set of dice in a given slot w/o regard for exogenous gamestate (bonuses, yahtzee wildcards etc) 
score_slot_with_dice(slot::Slot, sorted_dievals) ::u8 = let
    if slot==ACES return score_aces(sorted_dievals) end 
    if slot==TWOS return score_twos(sorted_dievals) end 
    if slot==THREES return score_threes(sorted_dievals) end 
    if slot==FOURS return score_fours(sorted_dievals) end 
    if slot==FIVES return score_fives(sorted_dievals) end 
    if slot==SIXES return score_sixes(sorted_dievals) end 
    if slot==THREE_OF_A_KIND return score_three_of_a_kind(sorted_dievals) end 
    if slot==FOUR_OF_A_KIND return score_four_of_a_kind(sorted_dievals) end 
    if slot==SM_STRAIGHT return score_sm_str8(sorted_dievals) end 
    if slot==LG_STRAIGHT return score_lg_str8(sorted_dievals) end 
    if slot==FULL_HOUSE return score_fullhouse(sorted_dievals) end 
    if slot==CHANCE return score_chance(sorted_dievals) end 
    if slot==YAHTZEE return score_yahtzee(sorted_dievals) end 
end

#=-------------------------------------------------------------
APP
-------------------------------------------------------------=#

struct App
    game:: GameState 
    ev_cache:: Vector{ChoiceEV}
    bar:: Progress 
end

# return a newly initialized app
function App(game::GameState) 
    lookups, saves = counts(game)
    bar = Progress(lookups, dt=1, showspeed=true) 
    ev_cache = Vector{ChoiceEV}(undef,2^30) # 2^30 bits hold all unique game states
    # sizehint!(ev_cache,saves)
    return App(game, ev_cache, bar)
end 

output_state_choice(self ::App, state ::GameState, choice_ev ::ChoiceEV) = let 
    # Uncomment below for more verbose progress output at the expense of speed 
    # println(state, choice_ev, Threads.threadid() ) #.printed(state, choice_ev)
end 



#=-------------------------------------------------------------
BUILD_CACHE
-------------------------------------------------------------=#
# gather up expected values in a multithreaded bottom-up fashion. this is like.. the main thing

function build_cache!(self::App) # = let 
    @inbounds all_dieval_combos=[o.dievals for o in OUTCOMES[outcomes_range_for_selection(0b11111)] ] # TODO backport this depature to python/rust?
    placeholder_dievals = DieVals(0) 
    placeholder_dievals_vec = [placeholder_dievals]
    false_true = (true, false); just_false = (false,)

    # first handle special case of the most leafy leaf calcs -- where there's one slot left and no rolls remaining
    for single_slot in self.game.open_slots   
        slot = Slots(Slot(single_slot)) # set of a single slot 
        joker_rules_in_play = single_slot!=YAHTZEE # joker rules in effect when the yahtzee slot is not open 
        for yahtzee_bonus_available in ifelse(joker_rules_in_play, false_true, just_false ) # yahtzee bonus -might- be available when joker rules are in play 
            for upper_total in useful_upper_totals(slot)
                for dieval_combo in all_dieval_combos
                    state = GameState(
                        dieval_combo, 
                        slot, 
                        upper_total, 
                        0, 
                        yahtzee_bonus_available
                    ) 
                    score = score_first_slot_in_context(state) 
                    choice_ev = ChoiceEV(single_slot, score)
                    self.ev_cache[state.id] = choice_ev
                    output_state_choice(self, state, choice_ev)
    end end end end 

    # for each length 
    for slots_len in 1:length(self.game.open_slots) 

        # for each slotset (of above length)
        for slots_vec in combinations(self.game.open_slots, slots_len) 
            slots::Slots = Slots(slots_vec...)
            joker_rules_in_play = !has(slots,YAHTZEE) # joker rules are in effect whenever the yahtzee slot is already filled 

            # for each upper total 
            for upper_total::u8 in useful_upper_totals(slots) 

                # for each yathzee bonus possibility 
                for yahtzee_bonus_available in ifelse(joker_rules_in_play, false_true, just_false ) # bonus always unavailable unless yahtzees are wild first

                    ticks = 848484 #=dice selection cache reads =# + (252 * slots_len * (2-(slots_len==1)) ) #= slot selection cache reads =#
                    update!(self.bar, self.bar.counter+ticks) # advance the progress bar by the number of cache reads coming up for dice selection 

                    # for each rolls remaining
                    for rolls_remaining in 0:3  

                        dieval_combos = ifelse(rolls_remaining==3 , placeholder_dievals_vec , all_dieval_combos)

                        # let built_from_threads = die_combos.into_par_iter().fold(YahtCache::default, |mut built_this_thread, die_combo|{  
                        # built_this_thread = YahtCache() #self.ev_cache #TODO come back to make this actually multithreaded like commented rust code above

                        Threads.@threads :static for dieval_combo in dieval_combos

                            # println(Threads.threadid() )
                            
                            process_dieval_combo!(
                                rolls_remaining, 
                                slots_len, 
                                slots, 
                                dieval_combo,
                                joker_rules_in_play,
                                yahtzee_bonus_available,
                                upper_total,
                                self,
                                placeholder_dievals
                            )

                        end # for die_combo in die_combos

                    end #for each rolls_remaining
                end #for each yahtzee_bonus_avail
            end #for each upper total 
        end #for each slot_vec
    end #for each length

end #fn build_cache

process_dieval_combo!(rolls_remaining, slots_len, slots, dieval_combo, joker_rules_in_play, yahtzee_bonus_available, upper_total, self, placeholder_dievals) = let

    threadid = Threads.threadid()

    if rolls_remaining==0  && slots_len > 1 # slot selection, but not leaf calcs already recorded

        #= HANDLE SLOT SELECTION  =# 

        slot_choice_ev=ChoiceEV(0,0)

        for slot in slots 

            #joker rules say extra yahtzees must be played in their matching upper slot if it's available
            first_dieval::u8 = dieval_combo[1]
            joker_rules_matter = joker_rules_in_play && score_yahtzee(dieval_combo)>0 && has(slots,first_dieval)
            head_slot::Slot = ifelse(joker_rules_matter , first_dieval , slot)

            yahtzee_bonus_avail_now = yahtzee_bonus_available
            upper_total_now::u8 = upper_total
            dievals_or_placeholder = dieval_combo
            head_plus_tail_ev = 0.f0
            rolls_remaining_now = 0

            # find the collective ev for the all the slots with this iteration's slot being first 
            # do this by summing the ev for the first (head) slot with the ev value that we look up for the remaining (tail) slots
            passes = slots_len==1 ? 1 : 2
            for i in 1:passes
                slots_piece = ifelse(i==1 , Slots(Slot(head_slot)) , remove(slots,head_slot))  # work on the head only or the set of slots without the head
                upper_total_to_save = ifelse(upper_total_now + best_upper_total(slots_piece) >= 63 , upper_total_now , 0x0)# only relevant totals are cached
                # if upper_total_now + best_upper_total(slots_piece) < 0x3f#=63=# upper_total_now = 0x0 end # map irrelevant totals to 0 when they can't reach the needed bonus threshold 
                state_to_get = GameState(
                    dievals_or_placeholder,
                    slots_piece, 
                    upper_total_to_save,
                    rolls_remaining_now, 
                    yahtzee_bonus_avail_now,
                )
                choice_ev = self.ev_cache[state_to_get.id]
                if i==1 && slots_len > 1 #prep 2nd pass on relevant 1st pass only..  
                    #going into tail slots next, we may need to adjust the state based on the head choice
                    if choice_ev.choice <= SIXES  # adjust upper total for the next pass 
                        added = u8(choice_ev.ev % 100) # the modulo 100 here removes any yathzee bonus from ev since that doesnt' count toward upper bonus total
                        upper_total_now = min(63, upper_total_now + added);
                    elseif choice_ev.choice==YAHTZEE  # adjust yahtzee related state for the next pass
                        if choice_ev.ev>0.f0 yahtzee_bonus_avail_now=true end
                    end 
                    rolls_remaining_now=3 # for upcoming tail lookup, we always want the ev for 3 rolls remaining
                    dievals_or_placeholder= placeholder_dievals # for 4 rolls remaining, use "wildcard" representative dievals since dice don't matter when rolling all of them
                end 
                head_plus_tail_ev += choice_ev.ev
            end #for i in passes 

            if head_plus_tail_ev >= slot_choice_ev.ev #TODO optimize with > instead of >= ?
                slot_choice_ev = ChoiceEV(slot, head_plus_tail_ev)
            end
            
            if joker_rules_matter break end # if joker-rules-matter we were forced to choose one slot, so we can skip trying the rest  

        end #for slot in slots                               

        state_to_set = GameState(
            dieval_combo,
            slots,
            upper_total, 
            0, 
            yahtzee_bonus_available,
        ) 
        self.ev_cache[state_to_set.id] = slot_choice_ev
        output_state_choice(self, state_to_set, slot_choice_ev)

    elseif rolls_remaining > 0  

    #= HANDLE DICE SELECTION =#    

        next_roll::u8 = rolls_remaining-1 
        best = ChoiceEV(0,0.)# selections are bitfields where '1' means roll and '0' means don't roll 
        selections = ifelse(rolls_remaining==3 , (0b11111:0b11111) , (0b00000:0b11111) )#select all dice on the initial roll, else try all selections
        
        for selection in selections # we'll try each selection against this starting dice combo  
            @inline avg_ev_for_selection = avg_ev(dieval_combo, selection, slots, upper_total, next_roll,yahtzee_bonus_available, self.ev_cache, threadid)
            if avg_ev_for_selection > best.ev
                best = ChoiceEV(selection, avg_ev_for_selection)
            end
        end 
        state_to_set = GameState(
            dieval_combo,
            slots, 
            upper_total, 
            rolls_remaining, 
            yahtzee_bonus_available, 
        ) 
        output_state_choice(self, state_to_set, best)
        self.ev_cache[state_to_set.id] = best

    end # if rolls_remaining...  

end
 
avg_ev(start_dievals, selection, slots, upper_total, next_roll,yahtzee_bonus_available, ev_cache, threadid) = let 

    total_ev_for_selection = 0.f0 
    outcomes_arrangements_count = 0.f0 
    range = outcomes_range_for_selection(selection) 
  
    @inbounds @simd ivdep for i in range  #"ivdep" breaks syntax checker but is valid macro arg which gives @simd some extra optimization leeway 
        NEWVALS_DATA_BUFFER[i, threadid] = start_dievals.data & OUTCOME_MASK_DATA[i]
        NEWVALS_DATA_BUFFER[i, threadid] |= OUTCOME_DIEVALS_DATA[i]
    end

    floor_state_id = GameState(
        DieVals(0),
        slots, 
        upper_total, 
        next_roll, # we'll average all the 'next roll' possibilities (which we'd calclated last) to get ev for 'this roll' 
        yahtzee_bonus_available, 
    ).id

    @inbounds for i in range 
        #= gather sorted =#
            newvals_datum = Int(NEWVALS_DATA_BUFFER[i, threadid])
            sorted_dievals_id = SORTED_DIEVALS[newvals_datum].id
        #= gather ev =#
            state_to_get_id = floor_state_id | sorted_dievals_id
            state_to_get_idx = Int(state_to_get_id)
            cache_entry = ev_cache[state_to_get_idx]
            OUTCOME_EVS_BUFFER[i, threadid] = cache_entry.ev
    end 

    @fastmath @inbounds @simd ivdep for i in range # we looped through die "combos" but we need to average all "perumtations"
        EVS_TIMES_ARRANGEMENTS_BUFFER[i, threadid] = OUTCOME_EVS_BUFFER[i, threadid] * OUTCOME_ARRANGEMENTS[i]
        total_ev_for_selection +=  EVS_TIMES_ARRANGEMENTS_BUFFER[i, threadid] 
        outcomes_arrangements_count += OUTCOME_ARRANGEMENTS[i] 
    end

    return  total_ev_for_selection / outcomes_arrangements_count

end
 
#=-------------------------------------------------------------
INITIALIZERS
-------------------------------------------------------------=#

struct DieValsID 
    dievals ::DieVals
    id ::u8
end

# for fast access later, this generates a sparse array of dievals in sorted form, 
# along with each's unique "ID" between 0-252, indexed by DieVals.data
sorted_dievals() ::OffsetVector{DieValsID} = begin 
    # vec = Vector{DieVals}(undef,32768)
    vec = OffsetVector{DieValsID}(undef,0:32767)
    vec[DieVals(0).data] = DieValsID(DieVals(0),0x0) # first one is for the special wildcard 
    for (i,combo) in enumerate( with_replacement_combinations(1:6,5) )
        for perm in permutations(combo,5) |> unique 
            vec[DieVals(perm...).data] = DieValsID(DieVals(combo...),i)
        end 
    end
    return vec
end


# count of arrangements that can be formed from r selections, chosen from n items, 
# where order DOES or DOESNT matter, and WITH or WITHOUT replacement, as specified.
n_take_r(n, r; order_matters=false, with_replacement=false) ::UInt = let
    if order_matters  #  order matters; we're counting "permutations" 
        if with_replacement 
            n^r
        else #  no replacement
            factorial(n) / factorial(n-r)  #  this = factorial(n) when r=n
        end 
    else #  we're counting "combinations" where order doesn't matter; there are less of these 
        if with_replacement 
            factorial(n+r-1) / (factorial(r)*factorial(n-1))
        else #  no replacement
            factorial(n) / (factorial(r)*factorial(n-r)) 
        end 
    end 
end 


# this generates the ranges that correspond to the outcomes, within the set of all outcomes, indexed by a give selection """
selection_ranges() = let  # ::Vector{UnitRange{Int}} 
    sel_ranges=Vector{UnitRange{Int}}(undef,32)
    s = 1
    sel_ranges[1] = 1:1 #TODO redundant?
    combos = powerset(1:5)
    for (i,combo) in enumerate(combos)
        count = n_take_r(6, length(combo), order_matters=false, with_replacement=true)
        sel_ranges[i] = s:(s+count-1)
        s += count
    end 
    return sel_ranges
end 

# preps the caches of roll outcomes data for every possible 5-die selection, where '0' represents an unselected die """
cache_roll_outcomes_data!() = let  
    i=0
    idx_combos = powerset(1:5) 
    for idx_combo_vec in idx_combos 
        dievals_vec = fill(DieVal(0),5)#Vector{DieVal}(0x0,5) 
        for dievals_combo_vec in with_replacement_combinations(1:6, length(idx_combo_vec))
            i+=1
            mask_vec = [0b111,0b111,0b111,0b111,0b111]
            for (j, val) in enumerate(dievals_combo_vec)
                idx = idx_combo_vec[j] 
                dievals_vec[idx] = DieVal(val) 
                mask_vec[idx]=DieVal(0)
            end 
            arrangement_count = distinct_arrangements_for(dievals_combo_vec)
            OUTCOME_DIEVALS_DATA[i] = DieVals(dievals_vec...).data
            OUTCOME_MASK_DATA[i] = DieVals(mask_vec...).data
            OUTCOME_ARRANGEMENTS[i] = arrangement_count
            OUTCOMES[i]=Outcome(
                DieVals(dievals_vec...),
                DieVals(mask_vec...),
                arrangement_count
            )
        end 
    end 
end 

distinct_arrangements_for(dieval_vec) ::f32 = let #(dieval_vec:Vec<DieVal>)->u8{
    key_count = counter(dieval_vec)
    divisor=1
    non_zero_dievals=0
    for (key, count) in key_count  
        if key != 0  
            divisor *= factorial(count)
            non_zero_dievals += count
        end  
    end  
    factorial(non_zero_dievals) / divisor
end 

# returns a range which corresponds the precomputed dice roll outcome data corresponding to the given selection
function outcomes_range_for_selection(selection::Selection) #(selection:u9)->&'static [Outcome]{
    one_based_idx = selection + 1 # selection bitfield is 0 to 31 but Julia indexes are from 1 to 32
    @inbounds idx = RANGE_IDX_FOR_SELECTION[one_based_idx]
    @inbounds range = SELECTION_RANGES[idx]
    return range
end

const SELECTION_RANGES = selection_ranges()  
const OUTCOMES = Vector{Outcome}(undef,1683) 
const OUTCOME_DIEVALS_DATA = Vector{u16}(undef,1683) 
const OUTCOME_MASK_DATA = Vector{u16}(undef,1683) 
const OUTCOME_ARRANGEMENTS = Vector{f32}(undef,1683) 
cache_roll_outcomes_data!()
const OUTCOME_EVS_BUFFER = Array{f32}(undef,1683,Threads.nthreads()) 
const NEWVALS_DATA_BUFFER = Array{u16}(undef,1683,Threads.nthreads()) 
const EVS_TIMES_ARRANGEMENTS_BUFFER = Array{f32}(undef,1683,Threads.nthreads())
const SORTED_DIEVALS = sorted_dievals()
const RANGE_IDX_FOR_SELECTION = [1,2,3,7,4,8,11,17,5,9,12,20,14,18,23,27,6,10,13,19,15,21,24,28,16,22,25,29,26,30,31,32] # julia hand-cobbled mapping
# const RANGE_IDX_FOR_SELECTION = [1,2,3,4,5,8,7,17,9,10,11,18,12,14,20,27,6,13,19,21,15,22,23,24,16,26,25,28,29,30,31,32] # mapping used in Rust and Python impls after 1-basing
# const RANGE_IDX_FOR_SELECTION = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32] # straight mapping  #TODO somehow these all work?

function main() 
    
    game = GameState( 
        # DieVals(1,2,3,5,6),
        DieVals(3,4,4,6,6),
        # DieVals(0),
        # Slots(0x4, 0x5, 0x6),
        Slots(0x1,0x2,0x8,0x9,0xa,0xb,0xc,0xd),
        # Slots(0x6),
        # Slots(0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xa,0xb,0xc,0xd), # should yield 254.5896
        # Slots(SIXES,YAHTZEE),
        # Slots(0x6,0x8,0xc), 
        # Slots(12),
        # Slots(0x3, FOURS, FIVES, SIXES, CHANCE, FULL_HOUSE, YAHTZEE, SM_STRAIGHT, LG_STRAIGHT, THREE_OF_A_KIND, FOUR_OF_A_KIND),
        # 0, 3, false
        0, 2, false
    )
    app = App(game)
    build_cache!(app)
    lhs=app.ev_cache[game.id]
    println("\n$(bitstring(lhs.choice))\t$(round(lhs.ev,digits=4))")
    # InteractiveUtils.@code_llvm avg_ev(
    #     game.sorted_dievals,
    #     0b11111,
    #     game.open_slots,
    #     game.upper_total,
    #     game.rolls_remaining,
    #     game.yahtzee_bonus_avail,
    #     app.ev_cache 
    # )
end

# main()


# end # modulne