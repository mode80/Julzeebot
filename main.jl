# import StaticArrays: SVector
import Combinatorics: permutations, with_replacement_combinations, combinations, powerset
import Base: convert
import DataStructures: counter
using Memoize
using Base
using ProgressMeter 

#=-------------------------------------------------------------
CONSTS, UTILS
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
mutable struct DieVals <: AbstractArray{DieVal, 1} #TODO make immutable to live on the stack in Julia
    data::u16 # 5 dievals, each from 0 to 6, can be encoded in 2 bytes total, each taking 3 bits
end

# convert(::Type{DieVals}, from::Vector{DieVal}) = DieVals(from) # avoid implicit for now

DieVals() = DieVals(0) 
 
DieVals(from ::Vector{T} ) where {T} = let
    DieVals(from...)
end

DieVals(d1::T, d2::T, d3::T, d4::T, d5::T) where {T} = let 
    DieVals(u16(d5) << 12 | u16(d4) << 9 | u16(d3) << 6 | u16(d2) << 3 | u16(d1))
end

# blit the 'from' dievals into the 'self' dievals with the help of a mask where 0 indicates incoming 'from' bits and 1 indicates none incoming 
blit!(self::DieVals, from::DieVals, mask::DieVals,) = # TODO make this into "blitted!" so DieVals can be immutable and live on the stack
    self.data = (self.data & mask.data) | from.data 

Base.copy(self::DieVals) = DieVals(self.data)

Base.IndexStyle(::Type{<:DieVals}) = IndexLinear()

Base.size(self::DieVals) = return (5,) 

Base.length(self::DieVals) = return 5 

Base.getindex(self::DieVals, i) ::DieVal = ((self.data >> ((i-1)*3)) & 0b111) 

Base.setindex!(self::DieVals, val::DieVal, i) = let #TODO removable for immutable DieVals? 
    bitpos = 3*(i-1) # widths of 3 bits per value
    mask = ~(UInt16(0b111) << bitpos) # hole maker
    self.data = (self.data & mask) | ( UInt16(val) << bitpos ) #  #  punch & fill hole
end

#=-------------------------------------------------------------
SortedSlots
-------------------------------------------------------------=#
struct SortedSlots <: AbstractArray{Slot, 1} 
    data::u16 # 13 sorted Slots can be positionally encoded in one u16
end

SortedSlots(iterable) = let 
    # @assert(length(v) <= 13)
    data::u16 = 0
    for slot in iterable 
        mask = 1 << u16(slot)
        data |= mask # force on
    end
    # for x in iterable; insert!(retval, x); end  
    return SortedSlots(data) 
end

Base.hash(self::SortedSlots, h::UInt) = hash(self.data)

Base.isequal(self::SortedSlots, other::SortedSlots) = isequal(self.data, other.data)

Base.iterate(self::SortedSlots, state=0) = let 
    while state < 13 
        state+=1
        if contains(self,state) return (Slot(state), state) end
    end 
    return nothing
end

Base.eltype(::Type{SortedSlots}) = Slot 

Base.length(self::SortedSlots) = count_ones(self.data) 

Base.size(self::SortedSlots) = (count_ones(self.data),)

Base.copy(self::SortedSlots) = SortedSlots(self.data)

Base.getindex(self::SortedSlots, i)::Slot= let
    # @assert(i<=length(self))
    bits = self.data
    bit_index=0
    for _ in 1:i  
        bit_index = trailing_zeros(bits)
        bits &= ~( 1 << u16(bit_index) )  #unset bit
    end
    return Slot(bit_index)
end

contains(self::SortedSlots, i) ::Bool = (self.data & (1<<u16(i)) > 0)

# insert!(self::SortedSlots, slots... ) = let
#     for slot in slots
#         mask = 1 << u16(slot)
#         self.data |= mask # force on
#     end
# end

# remove!(self::SortedSlots, slots... ) = let
#     for slot in slots
#         mask = ~( 1 << u16(slot) )
#         self.data &= mask # force off
#     end
# end

# Base.getindex(self::SortedSlots, i::Int)::Bool = contains(self,i) 

# Base.length(self::SortedSlots) = 13 

# Base.setindex!(self::SortedSlots, v::Bool, i::T) where {T<:Integer} =
#     if v
#         mask = 1 << u16(i)
#         self.data |= mask # force on
#     else
#         mask = ~( 1 << u16(i) );
#         self.data &= mask # force off
#     end

# Base.convert(::Type{SortedSlots}, v::Vector{Slot}) = SortedSlots(v)

previously_used_upper_slots(self::SortedSlots) ::SortedSlots = let
    all_bits_except_unused_uppers = ~self.data # "unused" slots (as encoded in .data) are not "previously used", so blank those out
    all_upper_slot_bits = u16((1<<7)-2)  # upper slot bits are those from 2^1 through 2^6 (.data encoding doesn't use 2^0)
    previously_used_upper_slot_bits = all_bits_except_unused_uppers & all_upper_slot_bits
    return SortedSlots( previously_used_upper_slot_bits )
end

# these are all the possible score entries for each upper slot
const UPPER_SCORES = ( 
    (0,0,0,0,0,0),      # STUB
    (0,1,2,3,4,5),      # ACES
    (0,2,4,6,8,10),     # TWOS
    (0,3,6,9,12,15),    # THREES 
    (0,4,8,12,16,20),   # FOURS
    (0,5,10,15,20,25),  # FIVES
    (0,6,12,18,24,30),  # SIXES
)

""" returns the unique and relevant "upper bonus total" that could have occurred from the previously used upper slots """
relevant_upper_totals(slots::SortedSlots) :: Vector{u8} = let ## TODO fix to this simplified version in the rust implmentation for fairness
    totals = Set(u8[])
    used_slot_idxs = previously_used_upper_slots(slots)
    slots_vals = (UPPER_SCORES[i] for i in used_slot_idxs) 
    used_score_perms = collect(Iterators.product(slots_vals...))
    for perm in used_score_perms
        tot = sum( perm )
        push!(totals, min(tot,63) )
    end 
    push!(totals,0) # 0 is always relevant and must be added here explicitly when there are no used upper slots 

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
    sorted_dievals ::DieVals
    sorted_open_slots ::SortedSlots 
    upper_total ::u8 = 0
    rolls_remaining ::u8 = 3 
    yahtzee_bonus_avail ::Bool = false
end 

Base.hash(self::GameState, h::UInt) = 
    hash(
        self.sorted_dievals, hash(
            self.sorted_open_slots, hash(
                self.upper_total, hash(
                    self.rolls_remaining, hash(
                        self.yahtzee_bonus_avail
    )))))

Base.isequal(self::GameState, other::GameState) = 
    isequal(self.sorted_dievals, other.sorted_dievals) && 
    isequal(self.sorted_open_slots, other.sorted_open_slots) && 
    isequal(self.upper_total, other.upper_total) && 
    isequal(self.rolls_remaining, other.rolls_remaining) && 
    isequal(self.yahtzee_bonus_avail, other.yahtzee_bonus_avail) 

 
# calculate relevant counts for gamestate: required lookups and saves
counts(self::GameState) :: Tuple{UInt,UInt} = let 
    lookups = 0 
    saves = 0 
    for subset_len in 1:length(self.sorted_open_slots)
        for slots_vec in combinations( collect(self.sorted_open_slots), subset_len )  
            slots = SortedSlots(slots_vec)
            joker_rules = contains(slots,YAHTZEE) # yahtzees aren't wild whenever yahtzee slot is still available 
            totals = relevant_upper_totals(slots) 
            for _ in totals 
                for __ in unique([false,joker_rules]) #
                    slot_lookups = (subset_len * ifelse(subset_len==1, 1, 2) ) * 252 #// * subset_len as u64;
                    dice_lookups = 848484 # // previoiusly verified by counting up by 1s in the actual loop. however chunking forward is faster 
                    lookups += dice_lookups + slot_lookups
                    saves+=1
    end end end end 
    return ( lookups, saves ) 
end 
                        
score_first_slot_in_context(self::GameState) ::u8 = let

    # score slot itself w/o regard to game state */
        slot = iterate(self.sorted_open_slots)[1]
        score = score_slot_with_dice(slot, self.sorted_dievals) 

    # add upper bonus when needed total is reached */
        if slot<=SIXES && self.upper_total>0  
            new_deficit = max(0,self.upper_total - score)
            if new_deficit==0 score += 35 end
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
        println("S, $(choice_ev.choice), $(state.sorted_dievals), $(state.rolls_remaining), $(state.upper_total), $(state.yahtzee_bonus_avail ? "Y" : ""), $(state.sorted_open_slots), $(choice_ev.ev)") 
    else 
        println("S, $(choice_ev.choice), $(state.sorted_dievals), $(state.rolls_remaining), $(state.upper_total), $(state.yahtzee_bonus_avail ? "Y" : ""), $(state.sorted_open_slots), $(choice_ev.ev)") 
        # println!("D,{:05b},{},{},{},{},{},{}",
        #     choice_ev.choice, state.sorted_dievals, state.rolls_remaining, state.upper_total, 
        #     state.yahtzee_bonus_avail ? "Y" : "", state.sorted_open_slots, choice_ev.ev) 
    end 
end 


#=-------------------------------------------------------------
SCORING FNs
-------------------------------------------------------------=#

score_upperbox(boxnum::Slot, sorted_dievals::DieVals) ::u8 = let #TODO rename SortedDieVals to DieValsID to avoid ambiguitiy in signatures like this one 
    sum::u8 = 0
    for d in sorted_dievals
        if d==boxnum sum+=boxnum end
    end
    return sum 
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

score_aces(sorted_dievals::       DieVals) ::u8         = score_upperbox(0x1,sorted_dievals)   
score_twos(sorted_dievals::       DieVals) ::u8         = score_upperbox(0x2,sorted_dievals) 
score_threes(sorted_dievals::     DieVals) ::u8         = score_upperbox(0x3,sorted_dievals) 
score_fours(sorted_dievals::      DieVals) ::u8         = score_upperbox(0x4,sorted_dievals) 
score_fives(sorted_dievals::      DieVals) ::u8         = score_upperbox(0x5,sorted_dievals) 
score_sixes(sorted_dievals::      DieVals) ::u8         = score_upperbox(0x6,sorted_dievals) 
    
score_three_of_a_kind(sorted_dievals::DieVals) ::u8     = score_n_of_a_kind(0x3,sorted_dievals) 
score_four_of_a_kind(sorted_dievals:: DieVals) ::u8     = score_n_of_a_kind(0x4,sorted_dievals) 
score_sm_str8(sorted_dievals::        DieVals) ::u8     = ifelse( straight_len(sorted_dievals) >= 0x4, 30, 0)
score_lg_str8(sorted_dievals::        DieVals) ::u8     = ifelse( straight_len(sorted_dievals) == 0x5, 40, 0)

# The official rule is that a Full House is "three of one number and two of another
score_fullhouse(sorted_dievals::DieVals) ::u8 = let
    counts = counter(sorted_dievals)
    if length(counts) != 2 return 0 end
    val1,val1count = iterate(counts)
    val2,val2count = iterate(counts)
    if val1==0 || val2==0 return 0 end
    if (val1count==3 && val2count==2) || (val2count==3 && val1count==2) return 25 else return 0x0 end
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
    return App(game, ev_cache, bar)
end 

output_state_choice(self ::App, state ::GameState, choice_ev ::ChoiceEV) = let 
    # Uncomment below for more verbose progress output at the expense of speed 
    # println(state, choice_ev) #.printed(state, choice_ev)
end 



#=-------------------------------------------------------------
BUILD_CACHE
-------------------------------------------------------------=#

# gather up expected values in a multithreaded bottom-up fashion. this is like.. the main thing
function build_cache!(self::App) # = let 
    all_die_combos=outcomes_for_selection(0b11111)
    placeholder_outcome = OUTCOMES[1] 

    # first handle special case of the most leafy leaf calcs -- where there's one slot left and no rolls remaining
    for single_slot in self.game.sorted_open_slots   
        slot = SortedSlots([single_slot]) # set of a single slot 
        joker_rules_in_play = single_slot!=YAHTZEE # joker rules in effect when the yahtzee slot is not open 
        for yahtzee_bonus_available in unique([false, joker_rules_in_play])  # yahtzee bonus -might- be available when joker rules are in play 
            for upper_total in relevant_upper_totals(slot)
                for outcome in all_die_combos
                    state = GameState(
                        rolls_remaining = 0, 
                        sorted_dievals = outcome.dievals, 
                        sorted_open_slots = slot, 
                        upper_total = upper_total, 
                        yahtzee_bonus_avail = yahtzee_bonus_available
                    ) 
                    score = score_first_slot_in_context(state) 
                    choice_ev = ChoiceEV(single_slot, score)
                    self.ev_cache[state] = choice_ev
                    output_state_choice(self, state, choice_ev)
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
                    update!(self.bar, self.bar.counter+(252 * slots_len * ifelse(slots_len==1, 1 ,2) ) ) # advance for slot selection cache reads

                    # for each rolls remaining
                    for rolls_remaining in 0:3  

                        die_combos = ifelse(rolls_remaining==3 , [placeholder_outcome] , all_die_combos)

                        # let built_from_threads = die_combos.into_par_iter().fold(YahtCache::default, |mut built_this_thread, die_combo|{  
                        # built_this_thread = YahtCache() #self.ev_cache #TODO come back to make this actually multithreaded like commented rust code above

                        for die_combo in die_combos

                            if rolls_remaining==0  

                                #= HANDLE SLOT SELECTION  =# 

                                slot_choice_ev=ChoiceEV(0,0)

                                for slot in slots 

                                    #joker rules say extra yahtzees must be played in their matching upper slot if it's available
                                    first_dieval = die_combo.dievals[1]
                                    joker_rules_matter = joker_rules_in_play && score_yahtzee(die_combo.dievals)>0 && contains(slots,first_dieval)
                                    head_slot::Slot = ifelse(joker_rules_matter , first_dieval , slot)
                                    head = SortedSlots([head_slot])

                                    yahtzee_bonus_avail_now = yahtzee_bonus_available
                                    upper_total_now = upper_total
                                    dievals_or_placeholder = die_combo.dievals 
                                    if slots_len > 1 # make the tail all but the head, or else just the head 
                                        headless = [s for s in slots if s != head_slot]
                                        tail = SortedSlots(headless) 
                                    else 
                                        tail = head 
                                    end
                                    head_plus_tail_ev = 0.0
    
                                    # find the collective ev for the all the slots with this iteration's slot being first 
                                    # do this by summing the ev for the first (head) slot with the ev value that we look up for the remaining (tail) slots
                                    rolls_remaining_now = 0
                                    for slots_piece in unique([head,tail])
                                        upper_total_now = ifelse(upper_total_now + best_upper_total(slots_piece) >= 63 , upper_total_now , 0)# only relevant totals are cached
                                        state = GameState(
                                            sorted_dievals = dievals_or_placeholder,
                                            sorted_open_slots= slots_piece, 
                                            upper_total= upper_total_now, 
                                            rolls_remaining = rolls_remaining_now, 
                                            yahtzee_bonus_avail= yahtzee_bonus_avail_now,
                                        )
                                        # cache = ifelse(slots_piece==head , leaf_cache , self.ev_cache) #TODO why need leaf_cache separate from main? how is this shared state read from multi threads??
                                        choice_ev = self.ev_cache[state]
                                        if slots_piece==head  # on the first pass only.. 
                                            #going into tail slots next, we may need to adjust the state based on the head choice
                                            if choice_ev.choice <= SIXES  # adjust upper total for the next pass 
                                                added = choice_ev.ev % 100; # the modulo 100 here removes any yathzee bonus from ev since that doesnt' count toward upper bonus total
                                                upper_total_now = min(63, upper_total_now + added);
                                            elseif choice_ev.choice==YAHTZEE  # adjust yahtzee related state for the next pass
                                                if choice_ev.ev>0.0 yahtzee_bonus_avail_now=true end
                                            end 
                                            rolls_remaining_now=3 # for upcoming tail lookup, we always want the ev for 3 rolls remaining
                                            dievals_or_placeholder= DieVals(0) # for 3 rolls remaining, use "wildcard" representative dievals since dice don't matter when rolling all of them
                                        end 
                                        head_plus_tail_ev += choice_ev.ev
                                    end #for slot_piece
                                    if head_plus_tail_ev >= slot_choice_ev.ev 
                                        slot_choice_ev = ChoiceEV(slot, head_plus_tail_ev)
                                    end
                                    
                                    if joker_rules_matter break end # if joker-rules-matter we were forced to choose one slot, so we can skip trying the rest  
                                end  
                                
                                state = GameState(
                                    sorted_dievals = die_combo.dievals,
                                    sorted_open_slots = slots,
                                    rolls_remaining = 0, 
                                    upper_total =upper_total, 
                                    yahtzee_bonus_avail = yahtzee_bonus_available,
                                ) 
                                self.ev_cache[state] = slot_choice_ev
                                output_state_choice(self, state, slot_choice_ev)

                            else #if rolls_remaining > 0  

                            #= HANDLE DICE SELECTION =#    

                                next_roll = rolls_remaining-1 
                                best_dice_choice_ev = ChoiceEV(0,0.)# selections are bitfields where '1' means roll and '0' means don't roll 
                                selections = ifelse(rolls_remaining==3 , (0b11111:0b11111) , (0b00000:0b11111) )#select all dice on the initial roll, else try all selections
                                for selection in selections  # we'll try each selection against this starting dice combo  
                                    total_ev_for_selection = 0.0 
                                    outcomes_count = 0 
                                    for roll_outcome in outcomes_for_selection(selection) 
                                        newvals = copy(die_combo.dievals)
                                        blit!(newvals, roll_outcome.dievals, roll_outcome.mask)
                                        # newvals = sorted[&newvals]; 
                                        state = GameState(
                                            sorted_dievals= SORTED_DIEVALS[newvals], 
                                            sorted_open_slots= slots, 
                                            upper_total= upper_total, 
                                            rolls_remaining= next_roll, # we'll average all the 'next roll' possibilities (which we'd calclated last) to get ev for 'this roll' 
                                            yahtzee_bonus_avail= yahtzee_bonus_available, 
                                        )
                                        ev_for_this_selection_outcome = 0. 
                                        try 
                                            ev_for_this_selection_outcome = self.ev_cache[state].ev 
                                        catch
                                            println(self.ev_cache)
                                            throw(Base.throw_code_point_err)
                                        end
                                        total_ev_for_selection += ev_for_this_selection_outcome * roll_outcome.arrangements # bake into upcoming average
                                        outcomes_count += roll_outcome.arrangements # we loop through die "combos" but we'll average all "perumtations"
                                    end  
                                    avg_ev_for_selection = total_ev_for_selection / outcomes_count
                                    if avg_ev_for_selection > best_dice_choice_ev.ev
                                        best_dice_choice_ev = ChoiceEV(selection, avg_ev_for_selection)
                                    end 
                                end 
                                state = GameState(
                                        sorted_dievals = die_combo.dievals,
                                        sorted_open_slots = slots, 
                                        upper_total =upper_total, 
                                        yahtzee_bonus_avail = yahtzee_bonus_available, 
                                        rolls_remaining = rolls_remaining, 
                                ) 
                                output_state_choice(self, state, best_dice_choice_ev)
                                self.ev_cache[state]=best_dice_choice_ev

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

#=-------------------------------------------------------------
INITIALIZERS
-------------------------------------------------------------=#

# #all possible sorted combos of 5 dievals (252 of them)
# dievals_for_dieval_id() ::Vector{DieVals} = begin 
#     out=Vector{DieVals}(undef,253)
#     out[1]=DieVals(0,0,0,0,0) # first one is the special wildcard 
#     for (i,combo) in enumerate( with_replacement_combinations(1:6,5) )
#         out[i+1]=DieVals(combo)
#     end 
#     return out
# end 

# dievals_id_for_dievals() ::Vector{DieValsID} = let 
#     arr = Vector{DieValsID}(undef,28087)
#     arr[1] = DieValsID(0) # first one is the special wildcard 
#     for (i,combo) in enumerate( with_replacement_combinations(1:6,5) )
#         for perm in permutations(combo,5) |> unique 
#             dievals = DieVals(perm) 
#             arr[dievals.data+1]= DieValsID(i+1) ;
#         end 
#     end
#     return arr
# end

sorted_dievals() ::Dict{DieVals} = let # TODO this could return a sparse array of only 2^5 = 32,768 u16s for faster lookups 
    dict = Dict{DieVals,DieVals}()
    sizehint!(dict,28087) 
    dict[DieVals(0)] = DieVals(0) # first one is for the special wildcard 
    for (i,combo) in enumerate( with_replacement_combinations(1:6,5) )
        for perm in permutations(combo,5) |> unique 
            dict[DieVals(perm)] = DieVals(combo)
        end 
    end
    return dict
end


# count of arrangements that can be formed from r selections, chosen from n items, 
# where order DOES or DOESNT matter, and WITH or WITHOUT replacement, as specified.
n_take_r(n, r, order_matters::Bool, with_replacement::Bool) ::UInt = let
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
selection_ranges() ::Vector{UnitRange{UInt}} = let  #todo check for 0-based to 1-based of by one errors
    sel_ranges=Vector{UnitRange{UInt}}(undef,32)
    s = 1
    sel_ranges[1] = 1:1 #TODO redundant?
    combos = die_index_combos()
    for (i,combo) in enumerate(combos)
        count = n_take_r(6, length(combo), false, true)
        sel_ranges[i] = s:(s+count-1)
        s += count
    end 
    sel_ranges
end 

# the set of roll outcomes for every possible 5-die selection, where '0' represents an unselected die """
all_selection_outcomes() ::Vector{Outcome} = let  
    retval = Vector{Outcome}(undef,1683) 
    i=0
    idx_combos = die_index_combos()
    for idx_combo in idx_combos 
        dievals = DieVals() 
        for dievals_combo in with_replacement_combinations(1:6, length(idx_combo))
            i+=1
            mask = DieVals(0b111,0b111,0b111,0b111,0b111)
            for (j, val) in enumerate(dievals_combo)
                idx = idx_combo[j] 
                dievals[idx] = DieVal(val) 
                mask[idx]=DieVal(0)
            end 
            arrangements = distinct_arrangements_for(dievals_combo)
            retval[i]=Outcome(copy(dievals),copy(mask),arrangements)
        end 
    end 
    return retval
end 

# the set of all ways to roll different dice, as represented by a collection of index arrays """
die_index_combos() = let #->[Vec<u8>;32]  { 
    return powerset(1:5)
end

distinct_arrangements_for(dieval_vec) ::u8 = let #(dieval_vec:Vec<DieVal>)->u8{
    counts = counter(dieval_vec)
    divisor=1
    non_zero_dievals=0
    for count in counts  
        if count[1] != 0  
            divisor *= factorial(count[2])
            non_zero_dievals += count[2]
        end  
    end  
    factorial(non_zero_dievals)
end 

# returns a slice from the precomputed dice roll outcomes that corresponds to the given selection bitfield """
outcomes_for_selection(selection::Selection) = let #(selection:u8)->&'static [Outcome]{
    one_based_idx = selection + 1 # selection bitfield is 0 to 31 but Julia indexes are from 1 to 32
    idx = RANGE_IDX_FOR_SELECTION[one_based_idx]
    range = SELECTION_RANGES[idx]
    OUTCOMES[range]
end

SELECTION_RANGES = selection_ranges()  
OUTCOMES = all_selection_outcomes()
SORTED_DIEVALS = sorted_dievals()
const RANGE_IDX_FOR_SELECTION = [1,2,3,4,5,8,7,17,9,10,11,18,12,14,20,27,6,13,19,21,15,22,23,24,16,26,25,28,29,30,31,32] # TODO corrected in light of Julia 1-based arrays

function main() 
    game = GameState( 
        rolls_remaining= 2,
        sorted_dievals= DieVals(3,4,4,6,6),
        sorted_open_slots= SortedSlots([6,12]), 
    )
    app = App(game)
    build_cache!(app)
    lhs=app.ev_cache[game]
    println("$lhs")
    # @assert(round(lhs.ev,digits=2) == 20.73)
end

main()
