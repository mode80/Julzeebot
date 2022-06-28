# import StaticArrays: SVector
import Combinatorics: permutations, with_replacement_combinations, combinations
import Base: convert
import DataStructures: counter
using Memoize
using Base
using ProgressMeter 
# import Base : @kwdef

#=-------------------------------------------------------------
CONSTS, UTILS, INITIALIZERS
-------------------------------------------------------------=#

const u8 = UInt8; u16 = UInt16; const f32=Float32; f64=Float64; # lazy rust-like abbreviations
const Choice = u8 # represents EITHER chosen scorecard Slot, OR a chosen dice Selection (below)
const Selection = u8 # a bitfield representing a selection of dice to roll (1 means roll, 0 means don't)
const DieVal = u8 # a single die value 0 to 6 where 0 means "unselected"
const Slot = u8

# a single scorecard slot with values ranging from ACES to CHANCE 
const ACES = 0x1; const TWOS = 0x2; const THREES = 0x3; const FOURS = 0x4; const FIVES = 0x5; const SIXES = 0x6;
const THREE_OF_A_KIND = 0x7; const FOUR_OF_A_KIND = 0x8; const FULL_HOUSE = 0x9; 
const SM_STRAIGHT = 0xA; const LG_STRAIGHT = 0xB; const YAHTZEE = 0xC; const CHANCE = 0xD;

# count of arrangements that can be formed from r selections, chosen from n items, 
# where order DOES or DOESNT matter, and WITH or WITHOUT replacement, as specified.
n_take_r(n::Int, r::Int, order_matters::Bool, with_replacement::Bool) = let
    if order_matters  #  order matters; we're counting "permutations" 
        if with_replacement 
            n^r
        else #  no replacement
            factorial(n) / factorial(n-r)  #  this = factorial(n] when r=n
        end 
    else #  we're counting "combinations" where order doesn't matter; there are less of these 
        if with_replacement 
            factorial(n+r-1) / (factorial(r)*factorial(n-1))
        else #  no replacement
            factorial(n) / (factorial(r)*factorial(n-r)) 
        end 
    end 
end 

#all possible sorted combos of 5 dievals (252 of them)
dievals_for_dieval_id() ::Vector{DieVals} = begin 
    out=Vector{DieVals}(undef,253)
    out[1]=[0,0,0,0,0] # first one is the special wildcard 
    for (i,combo) in enumerate( with_replacement_combinations(1:6,5) )
        out[i+1]=combo
    end 
    return out
end 

dievals_id_for_dievals() ::Vector{DieValsID} = let 
    arr = Vector{DieValsID}(undef,28087)
    arr[1] = DieValsID(0) # first one is the special wildcard 
    for (i,combo) in enumerate( with_replacement_combinations(1:6,5) )
        for perm in permutations(combo,5) |> unique 
            dievals = DieVals(perm) 
            arr[dievals.data]= DieValsID(i+1) ;
        end 
    end
    return arr
end

# this generates the ranges that correspond to the outcomes, within the set of all outcomes, indexed by a give selection """
selection_ranges() ::Vector{UnitRange{Int}} = let  #todo check for 0-based to 1-based of by one errors
    sel_ranges=Vector{UnitRange{Int}}(undef,32)
    s = 1
    sel_ranges[1] = 1:1 #TODO redundant?
    combos = die_index_combos()
    for (i,combo) in enumerate(combos)
        count = Int(n_take_r(6, length(combo), false, true))
        sel_ranges[i] = s:(s+count-1)
        s += count
    end 
    sel_ranges
end 

# the set of roll outcomes for every possible 5-die selection, where '0' represents an unselected die """
all_selection_outcomes() ::Vector{Outcome} = let  
    retval = Vector{Outcome}(undef,1683) 
    i=0
    for combo in die_index_combos()
        dievals = DieVals() 
        for dievals_vec in with_replacement_combinations(1:6, length(combo))
            mask = DieVals(0b111,0b111,0b111,0b111,0b111)
            for (j, val) in enumerate(dievals_vec)
                idx = combo[j] 
                dievals[idx] = val 
                mask[idx]=0
            end 
            arrangements = distinct_arrangements_for(dievals_vec)
            retval[i]=Outcome(dievals,mask,arrangements)
            i+=1
        end 
    end 
    return retval
end 

# the set of all ways to roll different dice, as represented by a collection of index arrays """
die_index_combos() = let #->[Vec<u8>;32]  { 
    them=[Vector{u8}() for _ in 1:32]
    i=1
    for n in 1:5 
        for combo in combinations(1:5, n) # changed from 0:4 in 0-based Python
            i+=1
            them[i]= combo 
        end 
    end 
    return them
end

distinct_arrangements_for(dieval_vec::Vector{DieVal}) ::u8 = let #(dieval_vec:Vec<DieVal>)->u8{
    counts = counter(dieval_vec)
    divisor=1
    non_zero_dievals=0
    for count in counts  
        if count[1] != 0  
            divisor *= factorial([count[2]])
            non_zero_dievals += count[2]
        end  
    end  
    factorial([non_zero_dievals])
end 


# returns a slice from the precomputed dice roll outcomes that corresponds to the given selection bitfield """
outcomes_for_selection(selection::Selection) = let #(selection:u8)->&'static [Outcome]{
    idx = SLOT_IDX_FOR_SELECTION[selection]
    range = copy(SELECTION_RANGES[idx])
    OUTCOMES[range]
end

 
SELECTION_RANGES = selection_ranges()  
OUTCOMES = all_selection_outcomes()
DIEVALS_ID_FOR_DIEVALS = dievals_id_for_dievals() #the compact (sorted) dieval id for every (unsorted?) 5-dieval-permutation-with-repetition
DIEVALS_FOR_DIEVALS_ID = dievals_for_dieval_id()
const SLOT_IDX_FOR_SELECTION = [1,2,3,4,5,8,7,17,9,10,11,18,12,14,20,27,6,13,19,21,15,22,23,24,16,26,25,28,29,30,31,32] # TODO corrected in light of Julia 1-based arrays

#=-------------------------------------------------------------
ChoiceEV
-------------------------------------------------------------=#
struct ChoiceEV 
    choice::Choice
    ev::Float32
end

#=-------------------------------------------------------------
DieVals
-------------------------------------------------------------=#
Base.@kwdef mutable struct DieVals <: AbstractArray{DieVal, 1} #TODO better if this can be immutable so it lives on stack in Julia
    data::u16 # 5 dievals, each from 0 to 6, can be encoded in 2 bytes total, each taking 3 bits
end

# convert(::Type{DieVals}, from::Vector{DieVal}) = DieVals(from) # avoid implicit for now

DieVals(from ::Vector{T} ) where {T<:Unsigned} = 
    DieVals(from...)

DieVals(d1::T, d2::T, d3::T, d4::T, d5::T) where {T<:Number} = 
    DieVals(d5 << 12 | d4 << 9 | d3 << 6 | d2 << 3 | d1)

IndexStyle(::Type{<:DieVals}) = IndexLinear()

size(self::DieVals) = return 5 

getindex(self::DieVals, i::Int) ::DieVal = ((self.data >> (i*3)) & 0b111) 

setindex!(self::DieVals, val::Int, i::Int) = begin
    bitpos = 3*i # widths of 3 bits per value
    mask = ! (UInt16(0b111) << bitpos) # hole maker
    self.data = (self.data & mask) | ( UInt16(val) << bitpos ) #  #  punch & fill hole
end

# blit the 'from' dievals into the 'self' dievals with the help of a mask where 0 indicates incoming 'from' bits and 1 indicates none incoming 
blit(self::DieVals, from::DieVals, mask::DieVals,) = let
    self.data = (self.data & mask.data) | from.data 
end

#=-------------------------------------------------------------
DieValsID
-------------------------------------------------------------=#
struct DieValsID <: AbstractArray{DieVal, 1}
    data::u8 # all 252 sorted dievals combos fit inside 8 bits 
end

DieVals(from::DieValsID) = DIEVALS_FOR_DIEVALS_ID[from.data] # note this is constructor for DIEVALS

DieValsID(d1::T, d2::T, d3::T, d4::T, d5::T)  where {T<:Number} = let 
    it = DieVals(d1,d2,d3,d4,d5)
    DIEVALS_ID_FOR_DIEVALS[it.data] 
end

DieValsID(from::Vector{DieVal}) = let 
    it = DieVals(from)
    DIEVALS_ID_FOR_DIEVALS[it.data] 
end

DieValsID(from::DieVals) = let 
    DIEVALS_ID_FOR_DIEVALS[from.data] 
end

# convert(::Type{SortedDieVals}, from::DieVals) = SORTED_DIEVALS_FOR_UNSORTED[from.data]  ## TODO avoid implicit conversion for now
# convert(::Type{DieVals}, from::SortedDieVals) = INDEXED_DIEVALS_SORTED[from.data]
IndexStyle(::Type{<:DieValsID}) = IndexLinear()

size(self::DieVals) = return 5 

getindex(self::DieValsID, i::Int) ::DieVal = convert(DieVals,self)[i] 

#=-------------------------------------------------------------
SortedSlots
-------------------------------------------------------------=#
mutable struct SortedSlots <: AbstractArray{Bool, 1} #TODO can we make this immutable for stack allocation in Julia?
    data::UInt16 # 13 sorted Slots can be positionally encoded in one u16
end

# SortedSlots(v::Vector{Any}) = SortedSlots(Slot[collect(v)...])
 
SortedSlots(v::Vector{Slot}) = let 
    @assert(length(v) <= 13)
    retval = SortedSlots(0)
    for x in v 
        retval[x]=true 
    end
    return retval 
end

Base.IndexStyle(::Type{<:SortedSlots}) = IndexLinear()

Base.size(self::SortedSlots) = return count_ones(self.data) # 16 - leading_zeros(self.data) 

Base.getindex(self::SortedSlots, i::Int)::Bool = self.data & (1<<i) > 0  

Base.setindex!(self::SortedSlots, v, i) =
    if v
        mask = 1<<i
        self.data |= mask # force on
    else
        mask = !(1<<i);
        self.data &= mask # force off
    end

Base.convert(::Type{SortedSlots}, v::Vector{Slot}) = SortedSlots(v)

previously_used_upper_slots(self::SortedSlots) ::SortedSlots = let
    return SortedSlots( (!self.data) & ((1<<7)-1) )
end

# these are all the possible score entries for each upper slot
const UPPER_SCORES = Vector{u8}[ 
    u8[0,0,0,0,0,0],      # STUB
    u8[0,1,2,3,4,5],      # ACES
    u8[0,2,4,6,8,10],     # TWOS
    u8[0,3,6,9,12,15],    # THREES 
    u8[0,4,8,12,16,20],   # FOURS
    u8[0,5,10,15,20,25],  # FIVES
    u8[0,6,12,18,24,30],  # SIXES
]

""" returns the unique and relevant "upper bonus total" that could have occurred from the previously used upper slots """
relevant_upper_totals(slots::SortedSlots) :: Vector{u8} = let ## TODO fix to this simplified version in the rust implmentation for fairness
    totals = Set(u8[])
    used_slot_idxs = previously_used_upper_slots(slots)
    slots_vals = [UPPER_SCORES[i] for i in used_slot_idxs] 
    used_score_perms = Iterators.product(slots_vals...)
    for perm in used_score_perms
        tot = sum( perm )
        totals.add(min(tot,63))
    end 
    totals.add(0) # 0 is always relevant and must be added here explicitly when there are no used upper slots 

    # filter out the totals that aren't relevant because they can't be reached by the upper slots remaining 
    # this filters out a lot of unneeded state space but means the lookup function must map extraneous deficits to a default 
    best_current_slot_total = best_upper_total(slots)
    return [x for x in totals if x==0 || x + best_current_slot_total >=63]
end

best_upper_total(self::SortedSlots) ::u8 = let
    sum=0
    for x in self  
        if x>6 break end
        sum+=x
    end
    sum*5
end


#=-------------------------------------------------------------
Outcome
-------------------------------------------------------------=#
Base.@kwdef struct Outcome  
    dievals::DieVals
    mask::DieVals # stores a pre-made mask for blitting this outcome onto a GameState.DieVals.data u16 later
    arrangements::u8  # how many indistinguisable ways can these dievals be arranged (ie swapping identical dievals)
end

#=-------------------------------------------------------------
GameState
-------------------------------------------------------------=#

Base.@kwdef struct GameState # TODO test impact of calling keyword funcs are bad for performance https://techytok.com/code-optimisation-in-julia/#keyword-arguments 
    dievals_id ::DieValsID = DieValsID(0,0,0,0,0)# 3bits per die unsorted =15 bits minimally ... 8bits if combo is stored sorted (252 possibilities)
    sorted_open_slots ::SortedSlots =SortedSlots(Slot[]) #  13 bits " 4 bits for a single slot 
    upper_total ::u8 = 0#  6 bits " 
    rolls_remaining ::u8 = 3 #  3 bits "
    yahtzee_bonus_avail ::Bool = false#  1 bit "
end #~500k for leafcalcs

#     /// calculate relevant counts for gamestate: required lookups and saves
counts(self::GameState) :: Tuple{Int,Int} = let 
    lookups = 0 
    saves = 0 
    for subset_len in 1:length(self.sorted_open_slots)
        for slots_vec in combinations( collect(self.sorted_open_slots), subset_len )  
            slots = SortedSlots(slots_vec)
            joker_rules = contains(Vector{Slot}(slots),YAHTZEE) # yahtzees aren't wild whenever yahtzee slot is still available 
            for _ in relevant_upper_totals(slots) 
                for __ in unique([false,joker_rules]) #
                    slot_lookups = (subset_len * subset_len==1 ? 1 : 2 ) * 252 #// * subset_len as u64;
                    dice_lookups = 848484 # // previoiusly verified by counting up by 1s in the actual loop. however chunking forward is faster 
                    lookups += dice_lookups + slot_lookups
                    saves+=1
    end end end end 
    return ( lookups, saves ) 
end 
                        
score_first_slot_in_context(self::GameState) ::u8 = let

    # score slot itself w/o regard to game state */
        slot = iterate(self.sorted_open_slots)    
        score = score_slot_with_dice(slot, self.dievals_id) 

    # add upper bonus when needed total is reached */
        if slot<=SIXES && self.upper_total>0  
            new_deficit = max(0,self.upper_total - score)
            if new_deficit==0 score += 35 end
        end  

    # special handling of "joker rules" */
        just_rolled_yahtzee = score_yahtzee(self.dievals_id)==50
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
        println("S, $(choice_ev.choice), $(state.dievals_id), $(state.rolls_remaining), $(state.upper_total), $(state.yahtzee_bonus_avail ? "Y" : ""), $(state.sorted_open_slots), $(choice_ev.ev)") 
    else 
        println("S, $(choice_ev.choice), $(state.dievals_id), $(state.rolls_remaining), $(state.upper_total), $(state.yahtzee_bonus_avail ? "Y" : ""), $(state.sorted_open_slots), $(choice_ev.ev)") 
        # println!("D,{:05b},{},{},{},{},{},{}",
        #     choice_ev.choice, state.sorted_dievals, state.rolls_remaining, state.upper_total, 
        #     state.yahtzee_bonus_avail ? "Y" : "", state.sorted_open_slots, choice_ev.ev) 
    end 
end 

output_state_choice(state ::GameState, choice_ev ::ChoiceEV) = let 
    # # Uncomment below for more verbose progress output at the expense of speed 
    # if state.rolls_remaining==0 
    #     self.bar.println(format!("S\t{: >6.2?}\t{:_^5}\t{:2?}\t{}\t{:2?}\t{}\t{: <29}",
    #         choice_ev.ev, choice_ev.choice, state.rolls_remaining, state.sorted_dievals, state.upper_total, 
    #         if state.yahtzee_bonus_avail {"Y"}else{""}, state.sorted_open_slots.to_string())); 
    # else
    #     self.bar.println(format!("D\t{: >6.2?}\t{:05b}\t{:2?}\t{}\t{:2?}\t{}\t{: <29}",
    #         choice_ev.ev, choice_ev.choice, state.rolls_remaining, state.sorted_dievals, state.upper_total, 
    #         if state.yahtzee_bonus_avail {"Y"}else{""}, state.sorted_open_slots.to_string())); 
    # end 
end 



#=-------------------------------------------------------------
SCORING FNs
-------------------------------------------------------------=#

score_upperbox(boxnum::Slot, sorted_dievals::DieVals) ::u8 = let #TODO rename SortedDieVals to DieValsID to avoid ambiguitiy in signatures like this one 
    filter(x->x==boxnum, sorted_dievals) |> sum # TODO could be faster with short-circuiting since dievals is sorted
end 

score_n_of_a_kind(n::u8, sorted_dievals::DieVals) ::u8 = let 
    inarow=1; maxinarow=1; lastval=100; sum=0; 
    for x in sorted_dievals 
        if (x==lastval && x!=0) inarow +=1 else inarow=1 end
        maxinarow = max(inarow,maxinarow)
        lastval = x
        sum+=x
    end 
    if (maxinarow>=n) return sum else return 0 end
end 

straight_len(sorted_dievals::DieVals) ::u8 = let
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

score_aces(sorted_dievals::       DieVals) ::u8         = score_upperbox(1,sorted_dievals)   #TODO reduce indirction by placing score_upper et all direct in arry?
score_twos(sorted_dievals::       DieVals) ::u8         = score_upperbox(2,sorted_dievals) 
score_threes(sorted_dievals::     DieVals) ::u8         = score_upperbox(3,sorted_dievals) 
score_fours(sorted_dievals::      DieVals) ::u8         = score_upperbox(4,sorted_dievals) 
score_fives(sorted_dievals::      DieVals) ::u8         = score_upperbox(5,sorted_dievals) 
score_sixes(sorted_dievals::      DieVals) ::u8         = score_upperbox(6,sorted_dievals) 
    
score_three_of_a_kind(sorted_dievals::DieVals) ::u8     = score_n_of_a_kind(3,sorted_dievals) 
score_four_of_a_kind(sorted_dievals:: DieVals) ::u8     = score_n_of_a_kind(4,sorted_dievals) 
score_sm_str8(sorted_dievals::        DieVals) ::u8     = (straight_len(sorted_dievals) >=4) ? 30 : 0
score_lg_str8(sorted_dievals::        DieVals) ::u8     = (straight_len(sorted_dievals) ==5) ? 40 : 0

# The official rule is that a Full House is "three of one number and two of another
score_fullhouse(sorted_dievals::DieVals) ::u8 = let
    counts = counter(sorted_dievals)
    if length(counts) != 2 return 0 end
    val1,val1count = iterate(counts)
    val2,val2count = iterate(counts)
    if val1==0 || val2==0 return 0 end
    if (val1count==3 && val2count==2) || (val2count==3 && val1count==2) return 25 else return 0 end
end 
    
score_chance(sorted_dievals::DieVals) ::u8 = sum(sorted_dievals) 
    
score_yahtzee(sorted_dievals::DieVals) ::u8 =
    (sorted_dievals[1] == sorted_dievals[5] != 0) ? 50 : 0 

# reports the score for a set of dice in a given slot w/o regard for exogenous gamestate (bonuses, yahtzee wildcards etc) 
score_slot_with_dice(slot::Slot, sorted_dievals::DieVals) ::u8 = SCORE_FNS[slot](sorted_dievals) 

const SCORE_FNS = [
    score_aces, score_twos, score_threes, score_fours, score_fives, score_sixes, 
    score_three_of_a_kind, score_four_of_a_kind, score_fullhouse, score_sm_str8, score_lg_str8, score_yahtzee, score_chance, 
]

#=-------------------------------------------------------------
APP
-------------------------------------------------------------=#
const YahtCache = Dict{GameState,ChoiceEV}

mutable struct App
    game:: GameState 
    ev_cache:: YahtCache
    bar:: Progress 
end

# return a newly initialized app
function App(game::GameState) 
    lookups, saves = counts(game)
    bar = Progress(lookups, dt=1, showspeed=true) 
    ev_cache ::YahtCache = Dict() 
    sizehint!(ev_cache,saves)
    return App(game, bar, ev_cache)
end 

#=-------------------------------------------------------------
BUILD_CACHE
-------------------------------------------------------------=#

# gather up expected values in a multithreaded bottom-up fashion. this is like.. the main thing
function build_cache!(self::App) # = let 
    all_die_combos=outcomes_for_selection(0b11111)
    placeholder_dievals= OUTCOMES[0] #OUTCOMES[0] == [Dievals::default()]
    leaf_cache = YahtCache()

    # first handle special case of the most leafy leaf calcs -- where there's one slot left and no rolls remaining
    for single_slot in self.game.sorted_open_slots   
        slot = SortedSlots([single_slot]) # set of a single slot 
        joker_rules_in_play = single_slot!=YAHTZEE # joker rules in effect when the yahtzee slot is not open 
        for yahtzee_bonus_available in unique([false, joker_rules_in_play])  # yahtzee bonus -might- be available when joker rules are in play 
            for upper_total in relevant_upper_totals(slot)
                for outcome in all_die_combos
                    state = GameState(
                        rolls_remaining = 0, 
                        sorted_dievals = outcome.dievals.into(), 
                        sorted_open_slots = slot, 
                        upper_total = upper_total, 
                        yahtzee_bonus_avail = yahtzee_bonus_available
                    ) 
                    score = score_first_slot_in_context(state) 
                    choice_ev = ChoiceEV(single_slot, score)
                    leaf_cache[state] = choice_ev
                    output_state_choice(state, choice_ev)
    end end end end 

    # for each length 
    for slots_len in 1:length(self.game.sorted_open_slots) 

        # for each slotset (of above length)
        for slots_vec in combinations(self.game.sorted_open_slots, slots_len) 
            slots = SortedSlots(slots_vec)
            joker_rules_in_play = !contains(slots,YAHTZEE) # joker rules are in effect whenever the yahtzee slot is already filled 

            # for each upper total 
            for upper_total in relevant_upper_totals(slots) 

                # for each yathzee bonus possibility 
                for yahtzee_bonus_available in unique([false,joker_rules_in_play]) # bonus always unavailable unless yahtzees are wild first

                    update!(self.bar, self.bar.counter+848484) # advance the progress bar by the number of cache reads coming up for dice selection 
                    update!(self.bar, self.bar.counter+(252 * slots_len * (slots_len==1 ? 1 : 2) ) ) # advance for slot selection cache reads

                    # for each rolls remaining
                    for rolls_remaining in 0:3  

                        die_combos = rolls_remaining==3 ? placeholder_dievals : all_die_combos

                        # let built_from_threads = die_combos.into_par_iter().fold(YahtCache::default, |mut built_this_thread, die_combo|{  
                        built_this_thread = self.ev_cache #TODO come back to make this actually multithreaded like commented rust code above

                        for die_combo in die_combos

                            if rolls_remaining==0  

                                #= HANDLE SLOT SELECTION  =# 

                                slot_choice_ev=ChoiceEV(0,0)

                                for slot in slots 

                                    #joker rules say extra yahtzees must be played in their matching upper slot if it's available
                                    first_dieval = die_combo.dievals[0]
                                    joker_rules_matter = joker_rules_in_play && score_yahtzee(die_combo.dievals)>0 && contains(slots,first_dieval)
                                    head_slot::Slot = joker_rules_matter ? first_dieval : slot 
                                    head = SortedSlots(head_slot)

                                    yahtzee_bonus_avail_now = yahtzee_bonus_available
                                    upper_total_now = upper_total
                                    dievals_or_wildcard = die_combo.dievals 
                                    tail = copy(slots)
                                    if slots_len > 1 tail[head_slot] = false else tail = head end  # make the tail all but the head, or else just the head 
                                    head_plus_tail_ev = 0.0
    
                                    # find the collective ev for the all the slots with this iteration's slot being first 
                                    # do this by summing the ev for the first (head) slot with the ev value that we look up for the remaining (tail) slots
                                    rolls_remaining = 0
                                    for slots_piece in unique([head,tail])
                                        upper_total_now = (upper_total_now + best_upper_total(slots_piece) >= 63) ? upper_total_now : 0 # only relevant totals are cached
                                        state = GameState(
                                            rolls_remaining = rolls_remaining, 
                                            sorted_dievals= DieValsID(dievals_or_wildcard),
                                            sorted_open_slots= slots_piece, 
                                            upper_total= upper_total_now, 
                                            yahtzee_bonus_avail= yahtzee_bonus_avail_now,
                                        )
                                        cache = slots_piece==head ? leaf_cache : self.ev_cache #TODO why need leaf_cache separate from main? how is this shared state read from multi threads??
                                        choice_ev = cache[state]
                                        if slots_piece==head  # on the first pass only.. 
                                            #going into tail slots next, we may need to adjust the state based on the head choice
                                            if choice_ev.choice <= SIXES  # adjust upper total for the next pass 
                                                added = choice_ev.ev % 100; # the modulo 100 here removes any yathzee bonus from ev since that doesnt' count toward upper bonus total
                                                upper_total_now = min(63, upper_total_now + added);
                                            elseif choice_ev.choice==YAHTZEE  # adjust yahtzee related state for the next pass
                                                if choice_ev.ev>0.0 yahtzee_bonus_avail_now=true end
                                            end 
                                            rolls_remaining=3 # for upcoming tail lookup, we always want the ev for 3 rolls remaining
                                            dievals_or_wildcard = DieVals(0) # for 3 rolls remaining, use "wildcard" representative dievals since dice don't matter when rolling all of them
                                        end 
                                        head_plus_tail_ev += choice_ev.ev
                                    end #for slot_piece
                                    if head_plus_tail_ev >= slot_choice_ev.ev 
                                        slot_choice_ev = ChoiceEV(slot, head_plus_tail_ev)
                                    end
                                    
                                    if joker_rules_matter break end # if joker-rules-matter we were forced to choose one slot, so we can skip trying the rest  
                                end  
                                
                                state = GameState(
                                    sorted_dievals = DieValsID(die_combo.dievals),
                                    sorted_open_slots = slots,
                                    rolls_remaining = 0, 
                                    upper_total =upper_total, 
                                    yahtzee_bonus_avail = yahtzee_bonus_available ,
                                ) 
                                built_this_thread[state] = slot_choice_ev
                                # output_state_choice(state, slot_choice_ev)

                            else #if rolls_remaining > 0  

                            #= HANDLE DICE SELECTION =#    

                                next_roll = rolls_remaining-1 
                                best_dice_choice_ev = ChoiceEV(0,0.0)# selections are bitfields where '1' means roll and '0' means don't roll 
                                selections = rolls_remaining==3 ? (0b11111:0b11111) : (0b00000:0b11111) #select all dice on the initial roll, else try all selections
                                for selection in selections  # we'll try each selection against this starting dice combo  
                                    total_ev_for_selection = 0.0 
                                    outcomes_count = 0 
                                    for roll_outcome in outcomes_for_selection(selection) 
                                        newvals = die_combo.dievals
                                        blit(newvals, roll_outcome.dievals, roll_outcome.mask)
                                        # newvals = sorted[&newvals]; 
                                        state = GameState(
                                            sorted_dievals= DieValsID(newvals), #TODO dispense with casting indirection and just array lookup? 
                                            sorted_open_slots= slots, 
                                            upper_total= upper_total, 
                                            yahtzee_bonus_avail= yahtzee_bonus_available, 
                                            rolls_remaining: next_roll # we'll average all the 'next roll' possibilities (which we'd calclated last) to get ev for 'this roll' 
                                        )
                                        ev_for_this_selection_outcome = self.ev_cache[state].ev 
                                        total_ev_for_selection += ev_for_this_selection_outcome * roll_outcome.arrangements # bake into upcoming average
                                        outcomes_count += roll_outcome.arrangements # we loop through die "combos" but we'll average all "perumtations"
                                    end  
                                    avg_ev_for_selection = total_ev_for_selection / outcomes_count
                                    if avg_ev_for_selection > best_dice_choice_ev.ev
                                        best_dice_choice_ev = ChoiceEV(selection, avg_ev_for_selection)
                                    end 
                                end 
                                state = GameState(
                                        sorted_dievals = DieValsID(die_combo.dievals),
                                        sorted_open_slots = slots, 
                                        upper_total =upper_total, 
                                        yahtzee_bonus_avail = yahtzee_bonus_available, 
                                        rolls_remaining = rolls_remaining, 
                                ) 
                                # output_state_choice(state, best_dice_choice_ev)
                                built_this_thread[state]=best_dice_choice_ev

                            end # if rolls_remaining...  

                        #     built_this_thread

                        # }).reduce(YahtCache::default, |mut a,built_from_thread|{
                        #     a.extend(&built_from_thread); a 
                        # }); # end die_combos.par_into_iter() 

                        # self.ev_cache.extend(&built_from_threads);

                        end # for die_combo in die_combos

                    end #for each rolls_remaining
                end #for each yahtzee_bonus_avail
            end #for each upper total 
        end #for each slot_vec
    end #for each length

end #fn build_cache

function main() 
    game = GameState(
        sorted_dievals = DieValsID(3,4,4,6,6),
        sorted_open_slots = SortedSlots([SIXES,YAHTZEE]),
        rolls_remaining = 2, 
    )
    app = App(game)
    build_cache!(app)
    lhs = app.ev_cache[game]
    println("$lhs")
    @assert(round(lhs.ev,digits=2) == 20.73)
end

main()