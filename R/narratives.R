# ---------- RACE NARRATIVE GENERATOR ----------
# Generates deterministic, varied narratives for individual races.
# Accepts used_quotes to avoid repeating closing quotes across races.
# Returns list(narrative, quote_used) when called from build_all_narratives,
# or a plain string when called standalone.
generate_race_narrative <- function(race_row, completed_races, used_quotes = character()) {
  race_name <- race_row$race
  race_date <- format(race_row$race_date, "%B %d, %Y")
  has_data  <- !is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)

  # Deterministic picker: given a seed string + slot name, pick one item from a vector.
  # The slot name ensures different narrative slots don't all land on the same index.
  seed_hash <- sum(utf8ToInt(paste0(race_name, race_date)))
  pick_from <- function(opts, slot = "") {
    h <- seed_hash + sum(utf8ToInt(slot))
    opts[((h) %% length(opts)) + 1]
  }

  # Pick from a pool while avoiding already-used items. Falls back to
  # deterministic pick if all have been used.
  pick_unique <- function(opts, slot = "", used = character()) {
    available <- setdiff(opts, used)
    if (length(available) == 0) available <- opts
    h <- seed_hash + sum(utf8ToInt(slot))
    available[((h) %% length(available)) + 1]
  }

  # Maritime / sailing quotes pool (expanded to 40 for variety)
  maritime_quotes <- c(
    "\u201cTwenty years from now you will be more disappointed by the things you didn\u2019t do than by the ones you did. Sail away from the safe harbor.\u201d \u2014 Mark Twain",
    "\u201cThe pessimist complains about the wind; the optimist expects it to change; the realist adjusts the sails.\u201d \u2014 William Arthur Ward",
    "\u201cI can\u2019t control the wind, but I can adjust my sails.\u201d \u2014 Jimmy Dean",
    "\u201cThe sea, once it casts its spell, holds one in its net of wonder forever.\u201d \u2014 Jacques Cousteau",
    "\u201cA ship in harbor is safe, but that is not what ships are built for.\u201d \u2014 John A. Shedd",
    "\u201cThere is nothing more enticing, disenchanting, and enslaving than the life at sea.\u201d \u2014 Joseph Conrad",
    "\u201cThe cure for anything is salt water: sweat, tears, or the sea.\u201d \u2014 Isak Dinesen",
    "\u201cIt is not the ship so much as the skillful sailing that assures the prosperous voyage.\u201d \u2014 George William Curtis",
    "\u201cThe wind and the waves are always on the side of the ablest navigator.\u201d \u2014 Edmund Gibbon",
    "\u201cAny fool can carry on, but a wise man knows how to shorten sail in time.\u201d \u2014 Joseph Conrad",
    "\u201cTo reach a port we must set sail \u2014 sail, not tie at anchor \u2014 sail, not drift.\u201d \u2014 Franklin D. Roosevelt",
    "\u201cHe that would learn to pray, let him go to sea.\u201d \u2014 George Herbert",
    "\u201cWe must free ourselves of the hope that the sea will ever rest. We must learn to sail in high winds.\u201d \u2014 Aristotle Onassis",
    "\u201cLand was created to provide a place for boats to visit.\u201d \u2014 Brooks Atkinson",
    "\u201cIf one does not know to which port one is sailing, no wind is favorable.\u201d \u2014 Seneca",
    "\u201cI must go down to the seas again, to the lonely sea and the sky, and all I ask is a tall ship and a star to steer her by.\u201d \u2014 John Masefield",
    "\u201cOnly the guy who isn\u2019t rowing has time to rock the boat.\u201d \u2014 Jean-Paul Sartre",
    "\u201cSail away from the safe harbor. Catch the trade winds in your sails. Explore. Dream. Discover.\u201d \u2014 H. Jackson Brown Jr.",
    "\u201cThe ocean stirs the heart, inspires the imagination and brings eternal joy to the soul.\u201d \u2014 Wyland",
    "\u201cFor whatever we lose (like a you or a me), it\u2019s always ourselves we find in the sea.\u201d \u2014 E. E. Cummings",
    "\u201cA smooth sea never made a skilled sailor.\u201d \u2014 Franklin D. Roosevelt",
    "\u201cThe sea is everything. Its breath is pure and healthy. It is an immense desert, where man is never lonely.\u201d \u2014 Jules Verne",
    "\u201cIn one drop of water are found all the secrets of all the oceans.\u201d \u2014 Kahlil Gibran",
    "\u201cThe goal is not to sail the boat, but rather to help the boat sail herself.\u201d \u2014 John Rousmaniere",
    "\u201cNot all treasure is silver and gold, mate.\u201d \u2014 Captain Jack Sparrow",
    "\u201cYou can never cross the ocean until you have the courage to lose sight of the shore.\u201d \u2014 Christopher Columbus",
    "\u201cThe boat that won\u2019t be still is the boat that gets there.\u201d \u2014 Old sailing proverb",
    "\u201cThere are good ships and wood ships, ships that sail the sea, but the best ships are friendships, may they always be.\u201d \u2014 Irish proverb",
    "\u201cOut of sight of land the sailor feels safe. It is the beach that worries him.\u201d \u2014 Charles G. Davis",
    "\u201cThe days pass happily with me wherever my ship sails.\u201d \u2014 Joshua Slocum",
    "\u201cBeing in a ship is being in a jail, with the chance of being drowned.\u201d \u2014 Samuel Johnson",
    "\u201cRopes should not be used on something a knot cannot solve.\u201d \u2014 Old rigger\u2019s saying",
    "\u201cAt sea, I learned how little a person needs, not how much.\u201d \u2014 Robin Lee Graham",
    "\u201cIf a man must be obsessed by something, I suppose a boat is as good as anything, perhaps a bit better than most.\u201d \u2014 E. B. White",
    "\u201cThe sea hates a coward.\u201d \u2014 Eugene O\u2019Neill",
    "\u201cA sailor is an artist whose medium is the wind.\u201d \u2014 Webb Chiles",
    "\u201cThere\u2019s nothing \u2014 absolutely nothing \u2014 half so much worth doing as simply messing about in boats.\u201d \u2014 Kenneth Grahame",
    "\u201cFavorable winds don\u2019t come to those who wait. They come to those who hoist the sail.\u201d \u2014 Anonymous",
    "\u201cTo young men contemplating a voyage, I\u2019d say go.\u201d \u2014 Joshua Slocum",
    "\u201cBad weather makes good sailors, but lousy vacation photos.\u201d \u2014 Dock wisdom"
  )

  # ---- No track data ----
  if (!has_data) {
    no_data_opts <- c(
      paste0("No track data is available for ", race_name, " (",
             race_date, "). The GPS apparently took the day off \u2014 even electronics need a mental health day now and then."),
      paste0("Track data for ", race_name, " (", race_date,
             ") has gone the way of Amelia Earhart \u2014 vanished without a trace. The instruments were either napping or staging a quiet mutiny."),
      paste0("Alas, no track data exists for ", race_name, " (", race_date,
             "). The sailing happened, but the electrons that were supposed to record it apparently jumped ship."),
      paste0(race_name, " (", race_date,
             ") sailed off the grid entirely. The NMEA data must have mutinied somewhere around the start line. Perhaps the GPS didn\u2019t know where it was sailing either."),
      paste0("The logbook for ", race_name, " (", race_date,
             ") is conspicuously empty. Either the instruments staged a wildcat strike or the data fell overboard. The electronics clearly got too much salt water."),
      paste0(race_name, " on ", race_date,
             " left no digital footprint. The race happened \u2014 the GPS just wasn\u2019t invited. Perhaps the instruments decided to stay ashore and guard the dock."),
      paste0("No NMEA breadcrumbs for ", race_name, " (", race_date,
             "). The data gremlins ate this one. Somewhere, a hard drive is whistling innocently and refusing to make eye contact."),
      paste0(race_name, " (", race_date,
             ") exists only in the crew\u2019s collective memory, which, after a few post-race beverages, is about as reliable as the instruments were that day."),
      paste0("The digital trail for ", race_name, " (", race_date,
             ") is thinner than the breeze on a drifter day. The race was real; the data was aspirational."),
      paste0(race_name, " on ", race_date,
             " \u2014 proof that you can race a sailboat without leaving a single byte of evidence. The perfect crime, if the crime were sailing.")
    )
    narr <- pick_from(no_data_opts, "nodata")
    return(list(narrative = narr, quote_used = NA_character_))
  }

  n_completed <- nrow(completed_races)
  paragraphs  <- character()

  # ---- Paragraph 1: Overview, distance, placement ----
  p1_parts <- character()

  # Distance
  if (!is.na(race_row$length)) {
    len <- race_row$length
    all_len <- completed_races$length[!is.na(completed_races$length)]
    if (length(all_len) > 2) {
      pct <- mean(len >= all_len)
      short_opts <- c(
        "one of the shorter jaunts on the dance card \u2014 a quick tango with the bay",
        "a sprint by Wings\u2019 standards \u2014 barely enough time to finish the first thermos of coffee",
        "the nautical equivalent of a warm-up lap \u2014 short, sharp, and over before the sunscreen soaked in",
        "a compact course that rewarded quick thinking over brute endurance",
        "a quickie \u2014 blink and you\u2019d miss the windward mark",
        "short enough that the cooler stayed closed and the snacks stayed dry",
        "a brevity-is-the-soul-of-wit kind of course \u2014 Shakespeare would approve",
        "the type of race where the pre-race briefing takes longer than the actual sailing"
      )
      mid_low_opts <- c(
        "a mid-range cruise \u2014 long enough to settle in, short enough to stay hungry",
        "a course of modest ambition \u2014 not a day sail, not an odyssey, but somewhere in the agreeable middle",
        "enough distance to find a rhythm but not so much that the crew started rationing snacks",
        "a comfortable distance \u2014 the kind where you finish wanting just a little bit more",
        "long enough to justify the effort of rigging up, short enough to still make happy hour",
        "a course that fell squarely in the \u2018Goldilocks zone\u2019 of racing distances",
        "a solid medium-length run \u2014 the kind that doesn\u2019t make headlines but always delivers good racing",
        "the perfect length for a crew that brought exactly one sandwich and half a thermos"
      )
      mid_high_opts <- c(
        "a proper voyage that demanded endurance and more than a few granola bars",
        "a course with some real estate to it \u2014 the kind that separates the prepared from the optimistic",
        "a substantial outing that tested patience, provisions, and the second wind of every crew member",
        "long enough that the crew stopped asking \u201chow much farther?\u201d and started just sailing",
        "a legit distance \u2014 the sort of race where you earn your dinner twice",
        "enough course to make everyone on board question at least one life choice",
        "a race with genuine mileage \u2014 legs got sore, attention spans got tested, and the cooler got lighter",
        "the kind of distance where the conversation shifts from tactics to \u2018what\u2019s for dinner\u2019"
      )
      long_opts <- c(
        "one of the longest courses Wings has ever stared down \u2014 marathon territory",
        "an absolute beast of a course \u2014 the kind that earns you bragging rights at the dock",
        "the sort of distance that makes you question your life choices around mile two and feel heroic by the finish",
        "a proper expedition \u2014 if nautical miles were frequent flyer points, this one would earn an upgrade",
        "an endurance event disguised as a sailboat race \u2014 the crew basically commuted to work and back",
        "so long the crew considered applying for a change-of-address form mid-race",
        "a true test of stamina, snack planning, and the human bladder",
        "the kind of course that separates the sailors from the passengers and the optimists from the realists"
      )
      d <- if (pct < 0.25) pick_from(short_opts, "dist")
           else if (pct < 0.50) pick_from(mid_low_opts, "dist")
           else if (pct < 0.75) pick_from(mid_high_opts, "dist")
           else pick_from(long_opts, "dist")

      dist_frame <- pick_from(c(
        paste0("At ", len, " nautical miles, this was ", d, "."),
        paste0("The course measured ", len, " nm \u2014 ", d, "."),
        paste0(len, " nautical miles of racing ahead: ", d, "."),
        paste0("A ", len, "-mile course: ", d, "."),
        paste0(len, " nm from gun to finish \u2014 ", d, ".")
      ), "distframe")
      p1_parts <- c(p1_parts, dist_frame)
    } else {
      p1_parts <- c(p1_parts, pick_from(c(
        paste0("The course stretched ", len, " nautical miles across the bay."),
        paste0("At ", len, " nautical miles, the course was laid and the starting gun awaited."),
        paste0(len, " nautical miles from gun to finish \u2014 every one of them earned."),
        paste0("A ", len, "-mile track across Tampa Bay \u2014 distance enough to sort out the fleet."),
        paste0(len, " nautical miles of bay to cover, and every one of them counted.")
      ), "distearly"))
    }
  }

  # Duration
  if (!is.na(race_row$duration_hrs) && race_row$duration_hrs > 0) {
    hrs  <- floor(race_row$duration_hrs)
    mins <- round((race_row$duration_hrs - hrs) * 60)
    dur  <- if (hrs > 0) paste0(hrs, "h ", mins, "m") else paste0(mins, "m")
    dur_opts <- c(
      paste0("Wings battled the course for ", dur, " \u2014 every minute earned, none gifted."),
      paste0("From start to finish: ", dur, " of concentration, sail changes, and the occasional argument with the wind."),
      paste0("The clock ran for ", dur, " \u2014 a testament to persistence if nothing else."),
      paste0(dur, " on the water, which is exactly as long as it took for the crew to remember why they love this sport."),
      paste0("Total race time: ", dur, ". That\u2019s one way to skip dinner prep."),
      paste0("Wings was out there for ", dur, " \u2014 long enough for the crew to bond, bicker, and bond again."),
      paste0(dur, " of racing that felt like both five minutes and five hours, depending on when you checked the clock."),
      paste0("From gun to finish: ", dur, ". The crew earned every second of their post-race cold one.")
    )
    p1_parts <- c(p1_parts, pick_from(dur_opts, "duration"))
  }

  # Placement
  if (!is.na(race_row$place) && !is.na(race_row$fleet)) {
    place_num <- suppressWarnings(readr::parse_number(as.character(race_row$place)))
    fleet_n   <- as.integer(race_row$fleet)
    if (!is.na(place_num) && !is.na(fleet_n) && fleet_n > 0) {
      pct_place <- place_num / fleet_n

      first_opts <- c(
        paste0("Wings seized first place in a fleet of ", fleet_n,
               " \u2014 the navigator was clearly channeling Edmund Gibbon\u2019s \u201cablest navigator\u201d energy."),
        paste0("First across the line in a fleet of ", fleet_n,
               ". The crew left nothing on the table and the competition in their wake."),
        paste0("A bullet \u2014 first place out of ", fleet_n,
               " boats. The dock walk after this one came with a little extra swagger."),
        paste0("Wings claimed the top step in a ", fleet_n,
               "-boat fleet. The kind of result that makes the post-race beverage taste a little sweeter."),
        paste0("First of ", fleet_n,
               ". The crew nailed it \u2014 the kind of race where everything clicks and the competition just watches your transom."),
        paste0("A first-place finish in a fleet of ", fleet_n,
               " \u2014 the sailing equivalent of a mic drop, except you still have to fold the sails."),
        paste0("Wings took the gun in a field of ", fleet_n,
               ". If the ocean stirs the heart, this finish sent it into overdrive."),
        paste0("First place, fleet of ", fleet_n,
               ". The kind of result that\u2019s worth framing, or at least bringing up at every dinner party for the next six months.")
      )
      top_third_opts <- c(
        paste0("Crossing the line ", place_num, " out of ", fleet_n,
               " boats, Wings carved out a top-third finish. Not too shabby."),
        paste0("A ", place_num, "-of-", fleet_n,
               " finish \u2014 comfortably in the upper tier. The sailing was sharp today."),
        paste0("Finishing ", place_num, " in a fleet of ", fleet_n,
               ", Wings punched above average. The kind of result that earns a nod from the competition."),
        paste0(place_num, " out of ", fleet_n,
               " \u2014 a top-third finish. The sea cast its spell, and Wings answered."),
        paste0("A strong ", place_num, "-place finish in a ", fleet_n,
               "-boat fleet \u2014 proof that showing up prepared still counts for something."),
        paste0(place_num, " of ", fleet_n,
               ". Upper-third territory \u2014 where the air is cleaner and the tacking is crisper."),
        paste0("Wings slotted in at ", place_num, " out of ", fleet_n,
               " \u2014 the kind of result that deserves a respectable head nod, not quite a fist pump, but close."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               ". A top-third result that keeps the crew\u2019s morale tank well above empty.")
      )
      top_half_opts <- c(
        paste0("A ", place_num, "-place finish in a fleet of ", fleet_n,
               " \u2014 solidly in the top half. Skillful sailing was on display today."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               " boats. Not headline news, but a respectable showing \u2014 the margins were probably measured in seconds, not boat lengths."),
        paste0(place_num, " out of ", fleet_n,
               " \u2014 the upper half of the fleet, where the tactics are a little sharper. Room to improve, but nothing to apologize for."),
        paste0("A ", place_num, "-place finish in a ", fleet_n,
               "-boat field \u2014 today the adjustments landed Wings in the top half."),
        paste0(place_num, " of ", fleet_n,
               " \u2014 top half. Not the podium, not the back \u2014 the goldilocks zone of competitive sailing where you can see the leaders and smell their wake."),
        paste0("Wings landed at ", place_num, " in a field of ", fleet_n,
               ". Solidly above the median \u2014 like finishing a marathon faster than the guy who talked trash at the start."),
        paste0("A ", place_num, " of ", fleet_n,
               " result. The kind of finish that says \u2018we belong here\u2019 without needing to say it out loud."),
        paste0(place_num, " out of ", fleet_n,
               ". Top half of the fleet \u2014 where ambition meets execution, even if they don\u2019t always shake hands.")
      )
      lower_half_opts <- c(
        paste0("Placing ", place_num, " of ", fleet_n,
               " boats \u2014 not the finish the crew ordered. Lessons were learned, and they weren\u2019t cheap."),
        paste0("A ", place_num, "-of-", fleet_n,
               " result. Sometimes the bay wins. The crew will adjust for next time."),
        paste0(place_num, " out of ", fleet_n,
               " boats \u2014 the kind of finish that builds character and fuels quiet determination."),
        paste0("Finishing ", place_num, " in a fleet of ", fleet_n,
               ". Not the podium, but not the back of the pack either \u2014 small tactical calls made all the difference."),
        paste0(place_num, " of ", fleet_n,
               ". A humbling result, but as the old saying goes, the worst day sailing still beats the best day at the office. (The crew would like to stress-test that theory.)"),
        paste0("A ", place_num, "-place finish in a ", fleet_n,
               "-boat fleet. The scoreboard was unkind, but the racing was honest. Somewhere there\u2019s a lesson; the crew is still looking."),
        paste0(place_num, " of ", fleet_n,
               " \u2014 not the stuff of victory speeches, but the stuff of improvement plans, which are arguably more useful."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               ". The kind of result that gets filed under \u2018we\u2019ll get \u2019em next time\u2019 \u2014 a folder that\u2019s getting respectably thick.")
      )
      bottom_opts <- c(
        paste0("At ", place_num, " of ", fleet_n,
               ", this was what diplomats call a \u2018character-building experience.\u2019 Wings certainly did show up, and that\u2019s worth something."),
        paste0(place_num, " of ", fleet_n,
               " \u2014 not the result anyone drew up on the whiteboard. Some classrooms are tougher than others."),
        paste0("A ", place_num, "-place finish out of ", fleet_n,
               ". Sometimes the scoreboard is unkind. Wings got some sweat, tears, and sea today."),
        paste0("Finishing near the back at ", place_num, " of ", fleet_n,
               ". The sort of day where you remind yourself that it\u2019s about the journey, then immediately wonder who made up that saying."),
        paste0(place_num, " of ", fleet_n,
               ". On the bright side, Wings provided an excellent view of everyone else\u2019s transoms \u2014 a learning experience in boat design, if nothing else."),
        paste0("A ", place_num, " of ", fleet_n,
               " finish. The only trophy available was the one for most creative excuses at the dock. Competition was fierce for that one too."),
        paste0(place_num, " out of ", fleet_n,
               " \u2014 a result that the crew will reference exclusively as \u2018a challenging day\u2019 in all future conversations."),
        paste0("Finishing ", place_num, " of ", fleet_n,
               ". If sailing were graded on enthusiasm rather than elapsed time, Wings would have been on the podium.")
      )

      ptxt <- if (place_num == 1) pick_from(first_opts, "place")
              else if (pct_place <= 0.33) pick_from(top_third_opts, "place")
              else if (pct_place <= 0.50) pick_from(top_half_opts, "place")
              else if (pct_place <= 0.75) pick_from(lower_half_opts, "place")
              else pick_from(bottom_opts, "place")
      p1_parts <- c(p1_parts, ptxt)
    } else if (is.na(place_num)) {
      nonnum_opts <- c(
        paste0("Wings finished with a ", race_row$place, " in a fleet of ", fleet_n,
               " \u2014 an unconventional result, like finding a message in a bottle that just says \u2018good luck.\u2019"),
        paste0("The scoreboard reads \u2018", race_row$place, "\u2019 in a fleet of ", fleet_n,
               " \u2014 not your typical number, but then Wings has never been your typical boat."),
        paste0("A result of \u2018", race_row$place, "\u2019 out of ", fleet_n,
               ". The race committee had their reasons. Wings had the sea."),
        paste0("Wings notched a \u2018", race_row$place, "\u2019 in a fleet of ", fleet_n,
               ". The scoreboard is being creative today, and honestly, so was the racing."),
        paste0("The official result: \u2018", race_row$place, "\u2019 out of ", fleet_n,
               ". Sometimes racing defies numerical classification. This was one of those times.")
      )
      p1_parts <- c(p1_parts, pick_from(nonnum_opts, "place"))
    }
  }

  if (length(p1_parts) > 0) {
    opener_opts <- c(
      paste0(race_name, " on ", race_date, ". "),
      paste0(race_name, ", ", race_date, ". "),
      paste0(race_date, " \u2014 ", race_name, ". "),
      paste0("Race day: ", race_name, ", ", race_date, ". "),
      paste0(race_date, ". ", race_name, ". ")
    )
    paragraphs <- c(paragraphs,
                    paste0(pick_from(opener_opts, "opener"),
                           paste(p1_parts, collapse = " ")))
  }

  # ---- Paragraph 2: Speed & polar performance ----
  sp <- character()

  if (!is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)) {
    avg_sog <- round(race_row$avg_sog, 1)
    all_sog <- completed_races$avg_sog[!is.na(completed_races$avg_sog) &
                                        !is.nan(completed_races$avg_sog)]
    sog_pct <- if (length(all_sog) > 2) mean(race_row$avg_sog >= all_sog) else 0.5

    slow_opts <- c(
      "on the leisurely end of the spectrum \u2014 the sort of pace where dolphins lap you",
      "a contemplative speed, as if the boat itself was deep in thought",
      "the kind of pace that tests your patience more than your sail trim",
      "slow enough that the crew had time to rethink every tactical decision twice",
      "the maritime equivalent of being stuck behind a school bus \u2014 but with nicer views",
      "a pace best described as \u2018meditative\u2019 by optimists and \u2018painful\u2019 by everyone else",
      "slow enough that a passing pelican gave the crew a sympathetic look",
      "more scenic cruise than race pace \u2014 the bay was beautiful, at least"
    )
    below_avg_opts <- c(
      "a touch below the fleet\u2019s historical average \u2014 not embarrassing, just... modest",
      "slightly below Wings\u2019 usual clip \u2014 like showing up to a party fashionably late, but with less champagne",
      "on the conservative side of Wings\u2019 speed ledger \u2014 the conditions were stingy",
      "a half-step behind the historical pace, as if the bay was charging a toll",
      "below average, but in the \u2018gave it an honest effort\u2019 sense rather than the \u2018forgot to untie the mooring\u2019 sense",
      "not Wings\u2019 fastest showing, but the boat was moving and that\u2019s more than the dock can say",
      "a tick below the career norm \u2014 the kind of day where the wind had other plans",
      "modestly below par \u2014 like shooting a 73 at the golf course, respectable but not Instagram-worthy"
    )
    mid_opts <- c(
      "right in the middle of Wings\u2019 historical range \u2014 steady as she goes",
      "squarely in the median zone \u2014 the Goldilocks speed: not too fast, not too slow",
      "par for the course in Wings\u2019 career average \u2014 dependable if not dramatic",
      "a thoroughly average pace by Wings\u2019 standards, which is not a criticism \u2014 average is hard-earned out here",
      "the statistical center of Wings\u2019 speed universe \u2014 the mean in every sense",
      "textbook average \u2014 the kind of number that makes a statistician shrug and say \u2018yep\u2019",
      "neither a highlight reel nor a blooper reel \u2014 more of a \u2018season recap\u2019 kind of speed",
      "reliably mid-pack speed \u2014 the Honda Civic of sailing performances"
    )
    fast_opts <- c(
      "faster than most of Wings\u2019 outings \u2014 the hull was humming",
      "well above the historical norm \u2014 Wings was in the groove and the water knew it",
      "a pace that put Wings in the upper echelon of her own race history",
      "the kind of speed that makes the crew grin and the competition nervous",
      "Wings was cooking today \u2014 the wake was impressive and the knot meter was earning its keep",
      "a pace that suggests the crew had their coffee dialed in and their tactics sharper than usual",
      "appreciably quick \u2014 the kind of speed that turns heads on the racecourse and prompts dock questions",
      "above-average velocity that left the crew feeling like they actually know what they\u2019re doing"
    )
    fastest_opts <- c(
      "among the fastest performances in the logbook \u2014 a day for the record books",
      "one for the record books \u2014 Wings was absolutely flying by her own standards",
      "blazing fast in historical context \u2014 the kind of speed that makes the hull sing",
      "a top-shelf performance that put most previous outings to shame",
      "near the top of Wings\u2019 all-time speed charts \u2014 the crew was clearly channeling something special",
      "the kind of speed that makes you wonder if someone secretly installed a turbocharger below deck",
      "Wings\u2019 inner speedboat was showing \u2014 the kind of pace that leaves salt spray and smiles in equal measure",
      "an elite performance by Wings\u2019 standards \u2014 the sort of day that gets a gold star in the logbook"
    )

    desc <- if (sog_pct < 0.20) pick_from(slow_opts, "sog")
            else if (sog_pct < 0.40) pick_from(below_avg_opts, "sog")
            else if (sog_pct < 0.60) pick_from(mid_opts, "sog")
            else if (sog_pct < 0.80) pick_from(fast_opts, "sog")
            else pick_from(fastest_opts, "sog")

    peak_opts <- if (!is.na(race_row$max_sog)) {
      pk <- round(race_row$max_sog, 1)
      pick_from(c(
        paste0(" with a peak of ", pk, " knots (hold onto your hats)"),
        paste0(", topping out at ", pk, " knots in a moment of pure velocity"),
        paste0(" and a max burst of ", pk, " knots that briefly rattled the coffee mugs"),
        paste0(", hitting ", pk, " knots at the high-water mark"),
        paste0(", with a top speed of ", pk, " knots that had the speedometer doing a double-take"),
        paste0(" and a maximum of ", pk, " knots \u2014 the kind of burst that makes you check the rigging"),
        paste0(", peaking at ", pk, " knots for one glorious, white-knuckle moment"),
        paste0(", touching ", pk, " knots when the stars aligned and the puffs cooperated")
      ), "peak")
    } else ""

    sog_frame <- pick_from(c(
      paste0("Average speed over ground was ", avg_sog, " knots", peak_opts, " \u2014 ", desc, "."),
      paste0("Wings averaged ", avg_sog, " knots SOG", peak_opts, " \u2014 ", desc, "."),
      paste0("The GPS logged an average of ", avg_sog, " knots", peak_opts, ". That\u2019s ", desc, "."),
      paste0("SOG clocked in at ", avg_sog, " knots on average", peak_opts, " \u2014 ", desc, "."),
      paste0(avg_sog, " knots average SOG", peak_opts, ". In other words: ", desc, ".")
    ), "sogframe")
    sp <- c(sp, sog_frame)
  }

  if (!is.na(race_row$avg_stw) && !is.nan(race_row$avg_stw) &&
      !is.na(race_row$avg_sog) && !is.nan(race_row$avg_sog)) {
    diff <- round(race_row$avg_sog - race_row$avg_stw, 2)
    if (abs(diff) > 0.15) {
      if (diff > 0) {
        pos_opts <- c(
          paste0("a friendly current chipping in about ", abs(diff), " knots \u2014 free speed, the best kind"),
          paste0("Mother Nature\u2019s subsidy: roughly ", abs(diff), " knots of current boost, no engine required"),
          paste0("a favorable tide lending ", abs(diff), " knots \u2014 the kind of gift you don\u2019t question"),
          paste0("a helpful push of about ", abs(diff), " knots from the current \u2014 sometimes the sea is generous"),
          paste0("roughly ", abs(diff), " knots of free current assist \u2014 the bay\u2019s version of a tailwind on the highway"),
          paste0("a ", abs(diff), "-knot current bonus \u2014 the tide was clearly a Wings fan today"),
          paste0("about ", abs(diff), " knots of current charity \u2014 the kind of boost that makes you look better than you are"),
          paste0("a ", abs(diff), "-knot gift from the tide gods, who apparently owed Wings a favor")
        )
        sp <- c(sp, paste0("The SOG-STW gap reveals ", pick_from(pos_opts, "current"), "."))
      } else {
        neg_opts <- c(
          paste0("an adversarial current dragging things back by about ", abs(diff), " knots \u2014 the sea giveth and the sea taketh away"),
          paste0("the tide playing defense, stealing roughly ", abs(diff), " knots of hard-won boat speed"),
          paste0("an unfriendly current taxing the boat about ", abs(diff), " knots \u2014 sailing\u2019s version of a headwind on the freeway"),
          paste0("a current penalty of ", abs(diff), " knots \u2014 the bay extracting its toll for the privilege of racing"),
          paste0("about ", abs(diff), " knots of current working against Wings \u2014 like running on a treadmill set to incline"),
          paste0("roughly ", abs(diff), " knots of tidal resistance \u2014 the ocean\u2019s way of saying \u2018not so fast, skipper\u2019"),
          paste0("a ", abs(diff), "-knot current tax \u2014 apparently the bay has a toll booth nobody told the crew about"),
          paste0("the current clawing back about ", abs(diff), " knots \u2014 every inch of progress earned twice over")
        )
        sp <- c(sp, paste0("The SOG-STW gap reveals ", pick_from(neg_opts, "current"), "."))
      }
    }
  }

  if (!is.na(race_row$polar_perf_stw) && !is.nan(race_row$polar_perf_stw)) {
    pp <- round(race_row$polar_perf_stw, 2)
    season_start_yr <- suppressWarnings(as.integer(substr(race_row$season, 1, 4)))
    is_recent <- !is.na(season_start_yr) && season_start_yr >= 2025

    ptxt <- if (pp > 0.3 && !is_recent) {
      pick_from(c(
        paste0("Polar performance registered at +", pp,
               " knots above target. Given that the instruments were still being dialed in during this period, ",
               "this likely reflects a speed sensor calibration issue rather than genuine over-performance. ",
               "Early-season instrument readings should be taken with a grain of sea salt."),
        paste0("The polars show +", pp,
               " knots above target, but the speed sensors during this era were more aspirational than accurate. ",
               "Think of it as the instruments telling the crew what they wanted to hear. ",
               "Calibration is a journey, not a destination."),
        paste0("+", pp,
               " knots over polar targets \u2014 impressive on paper, but the paddle wheel was still in its \u2018creative interpretation\u2019 phase. ",
               "The polars are trustworthy; the sensor data from this period, less so."),
        paste0("Polar performance of +", pp,
               " knots above target. The instruments were still in their \u2018honeymoon phase\u2019 \u2014 everything looked great on paper. ",
               "Reality and the speed sensor weren\u2019t properly introduced until a later season."),
        paste0("+", pp,
               " knots over target, but the speed transducer during this era had the accuracy of a horoscope. ",
               "Enthusiastic, occasionally correct, and best enjoyed with skepticism.")
      ), "polar")
    } else if (pp > 0 && !is_recent) {
      pick_from(c(
        paste0("Polar performance was +", pp,
               " knots above target. In the early days of Wings\u2019 instrumentation, ",
               "positive polar numbers often pointed to uncalibrated speed sensors rather than superhuman sailing."),
        paste0("+", pp,
               " knots above polar target. Before the instruments were properly calibrated, ",
               "these readings were more decorative than diagnostic. Trust the trend, not the absolute number."),
        paste0("Polar performance of +", pp,
               " knots. In this pre-calibration era, the speed sensor had a tendency to flatter. ",
               "Take it with the same grain of sea salt you\u2019d apply to a fish story about \u2018the one that got away.\u2019"),
        paste0("+", pp,
               " knots above polars \u2014 but the speed sensor was still telling white lies at this point. ",
               "The boat was good; the data was fiction."),
        paste0("Polar performance of +", pp,
               " knots. The uncalibrated speed sensor from this era was basically a hype man \u2014 supportive, but unreliable.")
      ), "polar")
    } else if (pp > 0.3 && is_recent) {
      pick_from(c(
        paste0("Polar performance clocked in at +", pp,
               " knots above target \u2014 with properly calibrated instruments, this is a genuinely impressive result. ",
               "Wings was sailing faster than the polars predicted, and the crew deserves the credit."),
        paste0("+", pp,
               " knots above polar targets. With the instruments now dialed in, this is the real deal \u2014 ",
               "Wings was outrunning her own design specs. Somewhere, a naval architect is nodding approvingly."),
        paste0("Polar performance hit +", pp,
               " knots over target. Now that the sensors are trustworthy, numbers like these tell a genuine story: ",
               "the crew found speed the designers didn\u2019t promise."),
        paste0("+", pp,
               " knots above target with calibrated instruments \u2014 this isn\u2019t sensor noise, this is talent. ",
               "Wings was genuinely outperforming her theoretical ceiling."),
        paste0("Polar performance of +", pp,
               " knots. With the sensors finally telling the truth, this number is earned, not inherited. ",
               "The J112e designers said \u2018this fast\u2019; the crew said \u2018hold my beverage.\u2019")
      ), "polar")
    } else if (pp > 0 && is_recent) {
      pick_from(c(
        paste0("Polar performance was +", pp,
               " knots above target \u2014 a solid result now that the instruments are well-calibrated. ",
               "The crew squeezed out a little extra from the boat."),
        paste0("+", pp,
               " knots above polar targets \u2014 not earth-shattering, but a clean positive number with trustworthy instruments is always a good sign."),
        paste0("Polar performance of +", pp,
               " knots. With calibrated sensors, this modest over-performance is honest speed \u2014 earned by the crew, confirmed by the data."),
        paste0("+", pp,
               " knots above target. A small but legitimate edge over the polars \u2014 like finding a dollar in your jacket pocket. Not life-changing, but satisfying."),
        paste0("Polar performance: +", pp,
               " knots. The crew eked out more than the polars promised, which with calibrated instruments means they genuinely found a little extra.")
      ), "polar")
    } else if (pp > -0.3) {
      pick_from(c(
        paste0("Polar performance of ", pp,
               " knots \u2014 just a whisker below target. Close enough that the polars aren\u2019t losing sleep over it."),
        paste0("At ", pp,
               " knots relative to polars, Wings was kissing distance from target. A boat-length here, a puff there, and this number flips positive."),
        paste0("Polar performance of ", pp,
               " knots \u2014 essentially on the money. The margins at this level are measured in tenths, and tenths are measured in luck."),
        paste0(pp, " knots off polar target \u2014 close enough to call it a rounding error if you squint. The polars and the reality were in the same zip code."),
        paste0("Polar performance: ", pp,
               " knots. Functionally on target \u2014 the difference between this and \u2018nailed it\u2019 is about one well-timed puff.")
      ), "polar")
    } else if (pp > -0.7) {
      pick_from(c(
        paste0("At ", pp,
               " knots below polar targets, the boat had more in the tank. The conditions (or perhaps the crew\u2019s pre-race lunch choices) left some speed on the table."),
        paste0("Polar performance of ", pp,
               " knots below target \u2014 not disastrous, but the polars are gently clearing their throat. There was speed to be found."),
        paste0(pp, " knots under polar targets. The boat was capable of more, but the day had other ideas. ",
               "Sometimes the best-laid tactics meet a current that didn\u2019t read the playbook."),
        paste0(pp, " knots below target. The polars are quietly disappointed, like a parent watching you parallel park. There\u2019s room for improvement, and they both know it."),
        paste0("Polar performance of ", pp,
               " knots. Not awful, but the boat\u2019s theoretical performance is side-eyeing the crew right now.")
      ), "polar")
    } else {
      pick_from(c(
        paste0("Polar performance of ", pp,
               " knots below target \u2014 rough day at the office. Some days the curriculum is harder than others."),
        paste0("At ", pp,
               " knots below polar targets, this was a humbling outing. The boat\u2019s potential went largely unrealized \u2014 ",
               "like owning a sports car and getting stuck in traffic."),
        paste0(pp, " knots under target \u2014 a significant gap between what the polars promised and what the day delivered. But Wings showed up, which is always the first step."),
        paste0("Polar performance: ", pp,
               " knots. The polars and the actual performance had a disagreement today, and the polars won by a comfortable margin."),
        paste0(pp, " knots below target. The boat was designed to go faster than this; the ocean had a different syllabus. ",
               "Everyone got an education, though not the one they signed up for.")
      ), "polar")
    }
    sp <- c(sp, ptxt)
  }

  if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))

  # ---- Paragraph 3: Wind conditions ----
  wp <- character()

  if (!is.na(race_row$avg_tws) && !is.nan(race_row$avg_tws)) {
    drifter_opts <- c(
      "a drifter \u2014 the kind of day where you can hear the barnacles growing on the hull",
      "barely a whisper of wind \u2014 the sails hung like laundry and the crew practiced their patience",
      "a parking lot \u2014 the kind of conditions where the best strategy is to bring a good book",
      "glass-calm misery disguised as a race \u2014 the wind gods were clearly on vacation",
      "so light the telltales gave up and just dangled there, judging everyone",
      "the kind of breeze you\u2019d struggle to blow out a birthday candle with",
      "dead calm with occasional hopes \u2014 the meteorological equivalent of a rain dance that didn\u2019t work",
      "a day where the most productive crew member was the one who remembered to bring cards"
    )
    light_opts <- c(
      "light air that tested the crew\u2019s patience like a DMV waiting room \u2014 only with better scenery",
      "a zephyr at best \u2014 the kind of breeze that rewards finesse over horsepower",
      "gossamer conditions that demanded featherweight touch on the helm and the patience of a monk",
      "the definition of a \u2018tactician\u2019s day\u2019 \u2014 every puff was a decision and every lull a test",
      "light enough that boat handling mattered more than boat speed \u2014 finesse over firepower",
      "the kind of breeze where weight placement is a competitive advantage and the lightest crew member becomes MVP",
      "gentle conditions that separated the patient from the impatient faster than you\u2019d think",
      "a whispering breeze that rewarded the attentive and punished the distracted"
    )
    working_opts <- c(
      "a solid working breeze \u2014 the Goldilocks zone of racing conditions",
      "textbook racing weather \u2014 enough wind to move, not so much that things get exciting in the wrong way",
      "the sweet spot \u2014 steady breeze, honest sailing, and the kind of conditions that make you glad you own a boat",
      "pleasant and purposeful wind \u2014 the kind of day that reminds you why you took up sailing in the first place",
      "ideal conditions \u2014 the wind was cooperating, the sails were drawing, and nobody was complaining (a first)",
      "a textbook breeze that made everyone on board look competent, even the ones who were faking it",
      "the perfect working breeze \u2014 steady, reliable, and drama-free, which is more than you can say for most crew dynamics",
      "comfortable racing conditions \u2014 the sort of day where sailing looks easy from shore and feels earned from the cockpit"
    )
    fresh_opts <- c(
      "a healthy blow that kept everyone earning their rum rations",
      "a stiff breeze that put the boat on its ear and the crew on their toes",
      "breezy enough to warrant a second look at the reef points \u2014 the wind meant business",
      "the kind of conditions where the rail meat earns their keep and the foredeck crew earns hazard pay",
      "enough wind to make the boat feel alive and the crew feel necessary \u2014 the sweet spot of controlled chaos",
      "a proper breeze that separated the confident from the cautious, with Wings choosing the former",
      "the kind of wind where the spray hits your face and your smile gets wider",
      "brisk conditions that made the instruments dance and the crew pay attention"
    )
    heavy_opts <- c(
      "heavy air that separated the bold from the seasick",
      "a proper blow \u2014 the kind of wind that rearranges the cockpit and tests every piece of hardware on the boat",
      "serious breeze that demanded respect, solid seamanship, and a willingness to get very wet",
      "enough wind to make even experienced sailors double-check the rigging \u2014 Mother Nature was not messing around",
      "full-send conditions \u2014 the kind of day where the winch handles get a workout and the crew gets a story",
      "howling breeze that tested the boat, the gear, and the crew\u2019s commitment to the hobby",
      "big wind that turned sailing from a sport into a survival exercise with trophies",
      "the kind of conditions where you stop racing the other boats and start racing your own adrenaline"
    )

    wd <- if (race_row$avg_tws < 5) pick_from(drifter_opts, "wind")
          else if (race_row$avg_tws < 8) pick_from(light_opts, "wind")
          else if (race_row$avg_tws < 12) pick_from(working_opts, "wind")
          else if (race_row$avg_tws < 18) pick_from(fresh_opts, "wind")
          else pick_from(heavy_opts, "wind")

    gust <- if (!is.na(race_row$max_tws) && !is.nan(race_row$max_tws))
              pick_from(c(
                paste0(" with gusts to ", round(race_row$max_tws, 1), " knots"),
                paste0(" and puffs hitting ", round(race_row$max_tws, 1), " knots"),
                paste0(", gusting to ", round(race_row$max_tws, 1)),
                paste0(" with the occasional ", round(race_row$max_tws, 1), "-knot reality check"),
                paste0(", spiking to ", round(race_row$max_tws, 1), " in the puffs")
              ), "gust")
            else ""

    wind_frame <- pick_from(c(
      paste0("Wind averaged ", round(race_row$avg_tws, 1), " knots", gust, " \u2014 ", wd, "."),
      paste0("The breeze clocked in at ", round(race_row$avg_tws, 1), " knots average", gust, " \u2014 ", wd, "."),
      paste0("Conditions served up ", round(race_row$avg_tws, 1), " knots of wind on average", gust, ". In other words: ", wd, "."),
      paste0("Mean wind: ", round(race_row$avg_tws, 1), " knots", gust, ". Translation: ", wd, "."),
      paste0("The wind meter read ", round(race_row$avg_tws, 1), " knots average", gust, " \u2014 ", wd, ".")
    ), "windframe")
    wp <- c(wp, wind_frame)

    all_tws <- completed_races$avg_tws[!is.na(completed_races$avg_tws) &
                                        !is.nan(completed_races$avg_tws)]
    if (length(all_tws) > 2) {
      tws_pct <- mean(race_row$avg_tws >= all_tws)
      calm_comp <- c(
        "Relative to the fleet\u2019s history, this was one of the calmer days \u2014 sail trim and boat handling were king.",
        "Historically speaking, this ranked among the lighter-air outings \u2014 the kind of day where small gains compound.",
        "By Wings\u2019 historical standards, this was a mellow affair \u2014 finesse over force.",
        "In the context of Wings\u2019 racing career, this was a whisper \u2014 the kind of day that rewards the patient and punishes the heavy-footed.",
        "Among the calmer outings in the dataset \u2014 a day where the crew\u2019s touch mattered more than the crew\u2019s weight."
      )
      windy_comp <- c(
        "This was one of the windier races in the dataset \u2014 a day where wisdom meant knowing when to shorten sail.",
        "Historically, this ranks among the breezier outings \u2014 a day where the boat\u2019s limits and the crew\u2019s nerve were both tested.",
        "By the numbers, this was more wind than Wings usually sees \u2014 the sort of day that produces war stories and sail repair bills.",
        "One of the spicier wind days on record for Wings \u2014 the kind that gets talked about at the bar but not always enjoyed in the moment.",
        "Among the windiest outings in Wings\u2019 logbook \u2014 a day where preparation paid dividends and the faint of heart stayed home."
      )
      comp <- if (tws_pct < 0.25) pick_from(calm_comp, "windcomp")
              else if (tws_pct > 0.75) pick_from(windy_comp, "windcomp")
              else ""
      if (nzchar(comp)) wp <- c(wp, comp)
    }
  }

  if (!is.na(race_row$headsail) && nzchar(race_row$headsail)) {
    sail_opts <- c(
      paste0("The crew flew the ", race_row$headsail, " \u2014 chosen with the confidence of someone who checks the forecast twice."),
      paste0("Up front: the ", race_row$headsail, ". A deliberate choice that said everything about what the crew expected from the sky."),
      paste0("The ", race_row$headsail, " got the call \u2014 the right tool for the day\u2019s conditions, or at least the crew\u2019s best guess at them."),
      paste0("Headsail selection: ", race_row$headsail, ". In sailing, as in life, half the battle is showing up with the right gear."),
      paste0("The ", race_row$headsail, " went up the headstay \u2014 a sail choice made with cautious optimism and confirmed by the first beat."),
      paste0("Sail plan featured the ", race_row$headsail, " \u2014 because picking the right headsail is 10% science and 90% staring at the clouds while sipping coffee."),
      paste0("The ", race_row$headsail, " was the weapon of choice today \u2014 a decision the crew stood by, for better or worse."),
      paste0("Flying the ", race_row$headsail, ". The forecast said one thing, the crew believed another, and the sail bag was already open so here we are.")
    )
    wp <- c(wp, pick_from(sail_opts, "headsail"))
  }

  if (!is.na(race_row$helm) && nzchar(race_row$helm)) {
    helm_opts <- c(
      paste0(race_row$helm, " had the helm and the final say on which way the bow pointed."),
      paste0("At the wheel: ", race_row$helm, " \u2014 steering with conviction and hopefully a compass."),
      paste0(race_row$helm, " drove \u2014 every tack, every gybe, every lane change negotiated from behind the wheel."),
      paste0("The helm belonged to ", race_row$helm, " today, who guided Wings through whatever the bay threw their way."),
      paste0(race_row$helm, " was calling the shots from the helm \u2014 part driver, part strategist, part weather guesser."),
      paste0("Behind the wheel: ", race_row$helm, ". The person most likely to be blamed or credited, depending on the result."),
      paste0(race_row$helm, " steered Wings with the quiet confidence of someone who has already apologized for the last bad tack."),
      paste0("Helm duties fell to ", race_row$helm, " \u2014 a role that combines authority, responsibility, and an excellent excuse for not trimming sails.")
    )
    wp <- c(wp, pick_from(helm_opts, "helm"))
  }

  if (length(wp) > 0) paragraphs <- c(paragraphs, paste(wp, collapse = " "))

  # ---- Closing quote (deduplicated) ----
  quote <- pick_unique(maritime_quotes, "closingquote", used_quotes)
  paragraphs <- c(paragraphs, quote)

  list(narrative = paste(paragraphs, collapse = "\n\n"), quote_used = quote)
}

# ---------- SEASON NARRATIVE GENERATOR ----------
generate_season_narrative <- function(season_name, season_cal, track_data) {
  if (nrow(season_cal) == 0) return(paste0("No races found for the ", season_name, " season. The harbor was apparently too comfortable."))

  seed_hash <- sum(utf8ToInt(season_name))
  pick_from <- function(opts, slot = "") {
    h <- seed_hash + sum(utf8ToInt(slot))
    opts[((h) %% length(opts)) + 1]
  }

  n_races <- season_cal |>
    mutate(race_date = as.Date(start)) |>
    distinct(race, race_date) |>
    nrow()

  paragraphs <- character()

  # ---- Paragraph 1: Overview ----
  date_range <- paste0(
    format(min(season_cal$start, na.rm = TRUE), "%B %Y"),
    " to ",
    format(max(season_cal$start, na.rm = TRUE), "%B %Y")
  )
  series_list <- unique(season_cal$series[!is.na(season_cal$series) & nzchar(season_cal$series)])
  series_txt <- if (length(series_list) > 0)
    paste0(" across ", length(series_list), " series (", paste(series_list, collapse = ", "), ")")
  else ""

  total_nm <- sum(season_cal$length, na.rm = TRUE)
  nm_txt <- if (total_nm > 0)
    paste0(", logging ", round(total_nm, 1), " nautical miles in the process")
  else ""

  opener_opts <- c(
    paste0("The ", season_name, " season ran from ", date_range,
           " and featured ", n_races, " races", series_txt, nm_txt,
           ". Wings was thoroughly committed to the calendar \u2014 and the calendar was thoroughly committed to testing Wings."),
    paste0("From ", date_range, ", the ", season_name, " season delivered ", n_races, " races",
           series_txt, nm_txt,
           ". A full dance card and a busy crew \u2014 exactly how a racing season should look."),
    paste0(season_name, ": ", n_races, " races", series_txt, ", spanning ", date_range, nm_txt,
           ". Another season of salt, spray, and the occasional existential question about why we do this."),
    paste0("The ", season_name, " season stretched from ", date_range,
           " with ", n_races, " races on the books", series_txt, nm_txt,
           ". Wings showed up for every one of them, which is either dedication or stubbornness. Possibly both."),
    paste0("Season ", season_name, " covered ", date_range,
           " and packed in ", n_races, " races", series_txt, nm_txt,
           ". The crew accumulated experience, nautical miles, and a healthy collection of dock stories.")
  )
  paragraphs <- c(paragraphs, pick_from(opener_opts, "seasonopener"))

  # ---- Paragraph 2: Placement summary ----
  place_num <- suppressWarnings(as.numeric(season_cal$place))
  fleet_num <- suppressWarnings(as.numeric(season_cal$fleet))
  valid_place <- !is.na(place_num) & !is.na(fleet_num) & fleet_num > 0

  if (sum(valid_place) > 0) {
    avg_place <- round(mean(place_num[valid_place]), 1)
    avg_fleet <- round(mean(fleet_num[valid_place]), 1)
    wins <- sum(place_num[valid_place] == 1)
    top_half <- sum(place_num[valid_place] <= fleet_num[valid_place] / 2)

    p_parts <- paste0("Across ", sum(valid_place), " scored races, Wings averaged ",
                      avg_place, " place in an average fleet of ", avg_fleet, " boats.")

    if (wins > 0) {
      win_opts <- c(
        paste0(" Wings hoisted the victory flag ", wins, ifelse(wins == 1, " time", " times"),
               " \u2014 proof that persistence and preparation occasionally converge."),
        paste0(" ", wins, " first-place finish", ifelse(wins > 1, "es", ""),
               " this season \u2014 the kind of result that makes the early-morning rigging worth it."),
        paste0(" The crew brought home ", wins, " bullet", ifelse(wins > 1, "s", ""),
               ". Not bad for a boat that sometimes argues with its own instruments."),
        paste0(" ", wins, " win", ifelse(wins > 1, "s", ""),
               " on the season \u2014 each one earned, none gifted, all celebrated at the dock.")
      )
      p_parts <- paste0(p_parts, pick_from(win_opts, "wins"))
    }

    top_pct <- round(100 * top_half / sum(valid_place))
    humor_opts <- if (top_pct >= 75) c(
      "Consistency like that doesn\u2019t happen by accident \u2014 or does it?",
      "That kind of reliability is rare in sailing, relationships, and weather forecasts.",
      "A top-half rate that high suggests the crew knows what they\u2019re doing. Probably.",
      "At that rate, Wings is the reliable friend who always shows up on time and brings good snacks."
    ) else if (top_pct >= 50) c(
      "More hits than misses \u2014 the kind of season that keeps the crew coming back.",
      "A winning record, technically speaking. The trophy case may not be overflowing, but the trend line is friendly.",
      "Above .500 in top-half finishes \u2014 in baseball terms, that\u2019s a playoff contender. In sailing terms, that\u2019s a well-used cooler.",
      "Not dominant, but not dominated. The sweet spot of competitive sailing."
    ) else c(
      "A season of lessons. Every expert was once a beginner, and Wings is investing in the education.",
      "The results don\u2019t always match the effort, but the crew\u2019s commitment to showing up is unimpeachable.",
      "More bottom-half finishes than top-half, but each one came with a lesson the crew didn\u2019t have to pay tuition for.",
      "A tough season on the scoreboard. The crew\u2019s resilience account, however, is running a surplus."
    )
    p_parts <- paste0(p_parts, " Wings finished in the top half in ", top_half,
                      " out of ", sum(valid_place), " races (", top_pct, "%). ",
                      pick_from(humor_opts, "seasonhumor"))

    paragraphs <- c(paragraphs, p_parts)
  }

  # ---- Paragraph 3: Track data performance ----
  season_track <- track_data |>
    filter(!is.na(race), nzchar(race))

  if (nrow(season_track) > 0) {
    matched_track <- tibble()
    for (i in seq_len(nrow(season_cal))) {
      tr <- season_track |>
        filter(race == season_cal$race[i],
               datetime_local >= season_cal$start[i],
               datetime_local <= season_cal$end[i])
      matched_track <- bind_rows(matched_track, tr)
    }

    if (nrow(matched_track) > 0) {
      sp <- character()
      avg_sog <- mean(matched_track$sog_knots, na.rm = TRUE)
      avg_stw <- mean(matched_track$stw_knots, na.rm = TRUE)

      if (!is.nan(avg_sog)) {
        sog_opts <- c(
          paste0("Season average SOG was ", round(avg_sog, 1), " knots \u2014 the cruising speed of a boat with places to be."),
          paste0("Wings averaged ", round(avg_sog, 1), " knots SOG across the season \u2014 not breaking records, but definitely breaking a sweat."),
          paste0("The season\u2019s average SOG came in at ", round(avg_sog, 1), " knots. Respectable, repeatable, and a solid baseline to build on."),
          paste0("Across all tracked races this season, Wings maintained ", round(avg_sog, 1), " knots average SOG \u2014 steady as the crew\u2019s coffee consumption.")
        )
        sp <- c(sp, pick_from(sog_opts, "seasonsog"))
      }

      if (!is.nan(avg_stw) && !is.nan(avg_sog)) {
        diff <- round(avg_sog - avg_stw, 2)
        if (abs(diff) > 0.1) {
          current_opts <- if (diff > 0) c(
            paste0("A season-long SOG-STW differential of +", diff, " knots suggests the currents were generally in Wings\u2019 corner \u2014 free speed, graciously accepted."),
            paste0("The tides chipped in about +", diff, " knots on average across the season \u2014 the bay\u2019s way of saying \u2018you\u2019re welcome.\u2019"),
            paste0("Net current contribution: +", diff, " knots. The tide was more teammate than obstacle this season."),
            paste0("+", diff, " knots of current assist on average \u2014 sometimes the universe cooperates, and this season it mostly did.")
          ) else c(
            paste0("A SOG-STW gap of ", diff, " knots hints at currents that were, on balance, not exactly rooting for Wings."),
            paste0("The tides clawed back about ", abs(diff), " knots on average \u2014 the bay charging rent for the privilege of racing."),
            paste0("Net current: ", diff, " knots against. The tide was playing for the other team this season."),
            paste0(diff, " knots of current headwind across the season. The crew worked harder than the numbers suggest.")
          )
          sp <- c(sp, pick_from(current_opts, "seasoncurrent"))
        }
      }

      avg_tws <- mean(matched_track$tws_knots, na.rm = TRUE)
      if (!is.nan(avg_tws)) {
        wind_opts <- if (avg_tws < 6) c(
          "Light enough to make a Laser sailor weep.",
          "A light-air season that tested patience more than hardware.",
          "Breeze was at a premium this season \u2014 the wind gods were being stingy."
        ) else if (avg_tws < 10) c(
          "Enough breeze to keep things interesting without requiring heroics.",
          "A moderate-wind season \u2014 the kind that rewards good tactics over brute force.",
          "Pleasant racing conditions, on average. Nobody\u2019s complaining about the breeze."
        ) else if (avg_tws < 15) c(
          "Solid, reliable wind \u2014 the kind you\u2019d write home about.",
          "A well-winded season that gave the crew plenty to work with.",
          "Good, honest breeze all season long. The sails earned their keep."
        ) else c(
          "Plenty of wind \u2014 reef points were not decorative this season.",
          "A breezy season that kept the crew honest and the rigging under review.",
          "Big wind all season. The crew\u2019s dry-suit budget went up accordingly."
        )
        sp <- c(sp, paste0("Average wind speed across the season was ",
                           round(avg_tws, 1), " knots. ", pick_from(wind_opts, "seasonwind")))
      }

      avg_polar_stw <- mean(matched_track$Polar_Perf_STW, na.rm = TRUE)
      if (!is.nan(avg_polar_stw)) {
        season_start_yr <- suppressWarnings(as.integer(substr(season_name, 1, 4)))
        is_recent <- !is.na(season_start_yr) && season_start_yr >= 2025

        pp_opts <- if (avg_polar_stw > 0.2 && !is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets on average \u2014 however, instrument calibration during this earlier season was still being refined. ",
                 "Positive polar numbers from this period likely reflect speed sensor inaccuracies rather than genuine over-performance."),
          paste0("Polar performance averaged +", round(avg_polar_stw, 2),
                 " knots above target, but the speed sensor during this era was still telling tales. ",
                 "The polars are right; the paddle wheel was being generous.")
        ) else if (avg_polar_stw > 0 && !is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets \u2014 though in this earlier season the speed instruments were not yet properly calibrated, ",
                 "so the true performance was likely closer to (or below) target."),
          paste0("Polar performance: +", round(avg_polar_stw, 2),
                 " knots. Flattering, but the instruments from this era were essentially writing fan fiction about boat speed.")
        ) else if (avg_polar_stw > 0.2 && is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets on average, Wings was genuinely outperforming her design specs this season. ",
                 "With properly calibrated instruments, this is a result the crew can take real pride in."),
          paste0("Polar performance of +", round(avg_polar_stw, 2),
                 " knots with calibrated instruments \u2014 the crew was extracting speed the designers didn\u2019t advertise. Impressive stuff.")
        ) else if (avg_polar_stw > 0 && is_recent) c(
          paste0("At +", round(avg_polar_stw, 2),
                 " knots above polar targets with well-calibrated instruments, Wings was edging past her theoretical ceiling. ",
                 "Every fraction of a knot earned the hard way."),
          paste0("Polar performance: +", round(avg_polar_stw, 2),
                 " knots above target. With trustworthy sensors, this modest over-performance is the real deal \u2014 genuine speed, honestly measured.")
        ) else if (avg_polar_stw > -0.3) c(
          paste0("At ", round(avg_polar_stw, 2),
                 " knots relative to polar targets, Wings was sailing close to her design envelope \u2014 minor tuning could close the gap."),
          paste0("Polar performance of ", round(avg_polar_stw, 2),
                 " knots. Essentially on target \u2014 the gap between actual and theoretical is small enough to blame on weather rather than technique.")
        ) else c(
          paste0("At ", round(avg_polar_stw, 2),
                 " knots below polar targets, there\u2019s room to coax more speed from the hull. The potential is there; the execution is a work in progress."),
          paste0("Polar performance: ", round(avg_polar_stw, 2),
                 " knots below target. The boat knows how to go faster \u2014 the crew is still negotiating the terms.")
        )
        sp <- c(sp, pick_from(pp_opts, "seasonpolar"))
      }

      if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))
    }
  }

  # ---- Closing (varied, no hardcoded quote) ----
  closing_opts <- c(
    paste0("Another season in the books for Wings. The bay keeps teaching, and the crew keeps showing up for class."),
    paste0("Season ", season_name, " is one for the logbook. The miles are in the keel, the lessons are in the crew, and the stories are at the bar."),
    paste0("That\u2019s a wrap on ", season_name, ". The boat is faster, the crew is wiser, and the cooler is empty. All signs of a season well spent."),
    paste0("The ", season_name, " chapter closes with more experience, more data, and the same unshakeable belief that next season will be even better."),
    paste0("Another season, another stack of NMEA files. Wings\u2019 story is still being written, one race at a time."),
    paste0("Season ", season_name, " \u2014 proof that the best way to get better at sailing is to do more of it. And possibly to read the weather forecast more carefully."),
    paste0("The ", season_name, " season reminded everyone aboard Wings that racing is a process, not a destination. The process, so far, involves a lot of sunscreen."),
    paste0("With ", season_name, " in the rearview, Wings looks ahead. The data is building, the skills are sharpening, and the snack planning is improving \u2014 priorities in that order.")
  )
  paragraphs <- c(paragraphs, pick_from(closing_opts, "seasonclosing"))

  paste(paragraphs, collapse = "\n\n")
}

# ---------- OVERALL PERFORMANCE NARRATIVE GENERATOR ----------
generate_performance_narrative <- function(race_calendar, track_data) {
  if (nrow(race_calendar) == 0) return("No race data available to generate a performance report.")

  paragraphs <- character()
  seasons <- sort(unique(race_calendar$season[!is.na(race_calendar$season)]))
  n_seasons <- length(seasons)
  n_total_races <- race_calendar |>
    mutate(race_date = as.Date(start)) |>
    distinct(race, race_date) |>
    nrow()
  total_nm <- sum(race_calendar$length, na.rm = TRUE)

  # ---- Paragraph 1: The grand overview ----
  first_race <- min(race_calendar$start, na.rm = TRUE)
  last_race  <- max(race_calendar$start, na.rm = TRUE)
  span_months <- round(as.numeric(difftime(last_race, first_race, units = "days")) / 30.44)

  overview_opts <- c(
    paste0("Across ", n_seasons, " seasons and ", n_total_races, " races spanning roughly ",
           span_months, " months, Wings has covered ", round(total_nm, 1),
           " nautical miles of competitive racing from ",
           format(first_race, "%B %Y"), " through ", format(last_race, "%B %Y"),
           ". These seasons are about building experience \u2014 learning the boat, learning the crew, and learning the water. ",
           "The foundation is being laid; the performance chapter is being written one race at a time."),
    paste0("Wings\u2019 racing logbook now spans ", n_seasons, " seasons, ", n_total_races, " races, and ",
           round(total_nm, 1), " nautical miles \u2014 a ", span_months,
           "-month journey from ", format(first_race, "%B %Y"), " to ", format(last_race, "%B %Y"),
           ". The data tells a story of a crew investing in experience before chasing silverware. Every race adds to the foundation."),
    paste0("From ", format(first_race, "%B %Y"), " to ", format(last_race, "%B %Y"),
           ", Wings has raced ", n_total_races, " times across ", n_seasons, " seasons, covering ",
           round(total_nm, 1), " nautical miles in roughly ", span_months,
           " months. That\u2019s a lot of salt water, sunscreen, and tactical arguments \u2014 all in the service of getting faster.")
  )
  seed_hash <- sum(utf8ToInt("overall_performance"))
  pick_from <- function(opts, slot = "") {
    h <- seed_hash + sum(utf8ToInt(slot))
    opts[((h) %% length(opts)) + 1]
  }
  paragraphs <- c(paragraphs, pick_from(overview_opts, "overview"))

  # ---- Paragraph 2: Placement trajectory across seasons ----
  place_num_all <- suppressWarnings(as.numeric(race_calendar$place))
  fleet_num_all <- suppressWarnings(as.numeric(race_calendar$fleet))
  valid_all <- !is.na(place_num_all) & !is.na(fleet_num_all) & fleet_num_all > 0

  if (sum(valid_all) > 0) {
    overall_avg <- round(mean(place_num_all[valid_all]), 1)
    overall_fleet <- round(mean(fleet_num_all[valid_all]), 1)
    total_wins <- sum(place_num_all[valid_all] == 1)
    top_half_all <- sum(place_num_all[valid_all] <= fleet_num_all[valid_all] / 2)
    top_pct_all <- round(100 * top_half_all / sum(valid_all))

    # Per-season breakdown
    season_stats <- lapply(seasons, function(s) {
      sc <- race_calendar[race_calendar$season == s, ]
      pn <- suppressWarnings(as.numeric(sc$place))
      fn <- suppressWarnings(as.numeric(sc$fleet))
      v  <- !is.na(pn) & !is.na(fn) & fn > 0
      if (sum(v) == 0) return(NULL)
      list(
        season = s,
        avg_place = round(mean(pn[v]), 1),
        avg_fleet = round(mean(fn[v]), 1),
        wins = sum(pn[v] == 1),
        n_scored = sum(v),
        top_half_pct = round(100 * sum(pn[v] <= fn[v] / 2) / sum(v))
      )
    })
    season_stats <- Filter(Negate(is.null), season_stats)

    p2 <- paste0("Overall, Wings has averaged ", overall_avg, " place in an average fleet of ",
                 overall_fleet, " boats across ", sum(valid_all), " scored races, finishing in the top half ",
                 top_pct_all, "% of the time.")

    if (total_wins > 0) {
      win_opts <- c(
        paste0(" Wings has claimed ", total_wins, " first-place finish",
               ifelse(total_wins > 1, "es", ""), " \u2014 proof that the crew can find the front of the fleet when conditions align."),
        paste0(" ", total_wins, " bullet", ifelse(total_wins > 1, "s", ""),
               " in the record book \u2014 each one a reminder that the potential is real."),
        paste0(" The crew has tasted victory ", total_wins, " time", ifelse(total_wins > 1, "s", ""),
               ". Not a dynasty, but not a drought either.")
      )
      p2 <- paste0(p2, pick_from(win_opts, "overallwins"))
    }

    # Trend detection
    if (length(season_stats) >= 2) {
      first_avg <- season_stats[[1]]$avg_place
      last_avg  <- season_stats[[length(season_stats)]]$avg_place
      first_thp <- season_stats[[1]]$top_half_pct
      last_thp  <- season_stats[[length(season_stats)]]$top_half_pct

      trend <- if (last_avg < first_avg - 0.5 && last_thp > first_thp + 5)
        paste0("The trajectory is encouraging \u2014 average placement has improved from ",
               first_avg, " (", seasons[1], ") to ", last_avg, " (", seasons[length(seasons)],
               "), and top-half finishes have climbed from ", first_thp, "% to ", last_thp,
               "%. The crew is clearly sharpening their game.")
      else if (last_avg > first_avg + 0.5)
        paste0("Average placement has shifted from ", first_avg, " (", seasons[1],
               ") to ", last_avg, " (", seasons[length(seasons)],
               "). The competition has gotten tougher, but the crew is adapting. Growth isn\u2019t always linear.")
      else
        paste0("Placement has been remarkably consistent across seasons (", first_avg,
               " in ", seasons[1], " vs ", last_avg, " in ", seasons[length(seasons)],
               ") \u2014 steady as the North Star.")

      p2 <- paste0(p2, " ", trend)
    }

    # Per-season breakdown line
    season_lines <- sapply(season_stats, function(ss) {
      paste0(ss$season, ": avg ", ss$avg_place, " place, ",
             ss$wins, " win", ifelse(ss$wins != 1, "s", ""),
             ", top half ", ss$top_half_pct, "%")
    })
    p2 <- paste0(p2, " Season by season: ", paste(season_lines, collapse = "; "), ".")

    paragraphs <- c(paragraphs, p2)
  }

  # ---- Paragraph 3: Speed and polar evolution ----
  track_with_race <- track_data |>
    dplyr::filter(!is.na(race), nzchar(race))

  if (nrow(track_with_race) > 0) {
    sp <- character()

    # Match track data to calendar for per-season stats
    season_perf <- lapply(seasons, function(s) {
      sc <- race_calendar[race_calendar$season == s, ]
      matched <- tibble::tibble()
      for (i in seq_len(nrow(sc))) {
        tr <- track_with_race |>
          dplyr::filter(race == sc$race[i],
                        datetime_local >= sc$start[i],
                        datetime_local <= sc$end[i])
        matched <- dplyr::bind_rows(matched, tr)
      }
      if (nrow(matched) == 0) return(NULL)
      list(
        season = s,
        avg_sog = mean(matched$sog_knots, na.rm = TRUE),
        avg_tws = mean(matched$tws_knots, na.rm = TRUE),
        avg_polar_stw = mean(matched$Polar_Perf_STW, na.rm = TRUE),
        n_pts = nrow(matched)
      )
    })
    season_perf <- Filter(Negate(is.null), season_perf)

    overall_sog <- mean(track_with_race$sog_knots, na.rm = TRUE)
    overall_tws <- mean(track_with_race$tws_knots, na.rm = TRUE)
    overall_polar <- mean(track_with_race$Polar_Perf_STW, na.rm = TRUE)

    if (!is.nan(overall_sog)) {
      sog_opts <- c(
        paste0("Across all instrumented races, Wings\u2019 overall average SOG is ", round(overall_sog, 1), " knots."),
        paste0("The career average SOG stands at ", round(overall_sog, 1), " knots \u2014 the pace of a boat learning its own potential."),
        paste0("Wings has averaged ", round(overall_sog, 1), " knots SOG across the entire dataset \u2014 a baseline that\u2019s only going to improve.")
      )
      sp <- c(sp, pick_from(sog_opts, "overallsog"))
    }

    if (!is.nan(overall_tws)) {
      wind_desc <- if (overall_tws < 8) "generally light-air diet"
                   else if (overall_tws < 12) "moderate breeze menu"
                   else "hearty wind buffet"
      sp <- c(sp, paste0("Average wind across the entire dataset is ",
                         round(overall_tws, 1), " knots \u2014 a ", wind_desc, "."))
    }

    if (!is.nan(overall_polar)) {
      polar_desc <- if (overall_polar > 0)
        paste0("Overall polar performance across all seasons is +", round(overall_polar, 2),
               " knots above target, though this aggregate figure is influenced by earlier seasons ",
               "when instrument calibration was still being refined. The most recent seasons provide ",
               "the most trustworthy picture of how Wings truly performs against her polars.")
      else if (overall_polar > -0.3)
        paste0("At ", round(overall_polar, 2), " knots relative to polars overall, Wings is sailing close to her design envelope \u2014 not far off the mark at all.")
      else
        paste0("At ", round(overall_polar, 2), " knots below polar targets overall, there\u2019s untapped speed in the hull. The crew is still unlocking the boat\u2019s full potential.")
      sp <- c(sp, polar_desc)
    }

    # Polar trend across seasons
    if (length(season_perf) >= 2) {
      polar_vals <- sapply(season_perf, function(x) x$avg_polar_stw)
      sog_vals   <- sapply(season_perf, function(x) x$avg_sog)
      s_names    <- sapply(season_perf, function(x) x$season)

      if (!any(is.nan(polar_vals))) {
        season_yrs <- suppressWarnings(as.integer(substr(s_names, 1, 4)))
        recent_idx <- which(!is.na(season_yrs) & season_yrs >= 2025)
        older_idx  <- which(!is.na(season_yrs) & season_yrs < 2025)

        trend_parts <- character()

        if (length(older_idx) > 0 && any(polar_vals[older_idx] > 0)) {
          older_pos <- older_idx[polar_vals[older_idx] > 0]
          trend_parts <- c(trend_parts,
            paste0("Earlier seasons (",
                   paste(s_names[older_pos], collapse = ", "),
                   ") show positive polar numbers, but these likely reflect instruments that were not yet properly calibrated rather than genuine over-performance."))
        }

        if (length(recent_idx) > 0) {
          recent_avg <- round(mean(polar_vals[recent_idx]), 2)
          recent_detail <- paste(paste0(round(polar_vals[recent_idx], 2), " in ", s_names[recent_idx]), collapse = ", ")
          trend_parts <- c(trend_parts,
            paste0("The most recent seasons (", recent_detail,
                   ") provide the most reliable benchmark with properly calibrated instruments, ",
                   "averaging ", recent_avg, " knots relative to polar targets."))
        }

        if (length(trend_parts) > 0) sp <- c(sp, paste(trend_parts, collapse = " "))
      }
    }

    if (length(sp) > 0) paragraphs <- c(paragraphs, paste(sp, collapse = " "))
  }

  # ---- Closing ----
  closing_opts <- c(
    paste0("From the first starting gun to the latest finish line, Wings\u2019 journey across ", n_seasons,
           " seasons tells a story of a crew investing in experience before chasing trophies. Every race sharpens the instincts, every mile deepens the understanding. ",
           "The exploration phase is well underway \u2014 and the data is building the roadmap for what comes next."),
    paste0(n_seasons, " seasons of racing. ", n_total_races, " starting guns. ", round(total_nm, 1),
           " nautical miles of lessons, wins, near-misses, and the occasional creative excuse. ",
           "The crew isn\u2019t done yet \u2014 if anything, they\u2019re just getting warmed up."),
    paste0("The Wings project continues. ", n_seasons, " seasons of data suggest a crew that\u2019s learning, adapting, and slowly turning experience into results. ",
           "The best races may still be ahead \u2014 the logbook is far from full.")
  )
  paragraphs <- c(paragraphs, pick_from(closing_opts, "overallclosing"))

  paste(paragraphs, collapse = "\n\n")
}

build_all_narratives <- function(data_rds) {
  track <- data_rds$track_all
  cal   <- data_rds$race_calendar

  if (nrow(cal) == 0) return(list())

  # Build per-race stats using the calendar's start/end to filter track data.
  cal_rows <- cal |>
    mutate(race_date = as.Date(start)) |>
    group_by(race, race_date) |>
    summarise(
      season   = first(season),
      series   = first(series),
      place    = first(place),
      fleet    = first(fleet),
      length   = first(length),
      helm     = first(helm),
      headsail = if ("headsail" %in% names(cal)) first(headsail) else NA_character_,
      cal_start = min(start, na.rm = TRUE),
      cal_end   = max(end, na.rm = TRUE),
      .groups  = "drop"
    )

  all_races <- cal_rows |>
    rowwise() |>
    mutate(
      track_subset = list({
        tr <- track |>
          filter(race == .env$race,
                 datetime_local >= cal_start,
                 datetime_local <= cal_end)
        if (nrow(tr) == 0) NULL else tr
      }),
      avg_sog        = if (is.null(track_subset)) NA_real_ else mean(track_subset$sog_knots, na.rm = TRUE),
      max_sog        = if (is.null(track_subset)) NA_real_ else max(track_subset$sog_knots, na.rm = TRUE),
      avg_stw        = if (is.null(track_subset)) NA_real_ else mean(track_subset$stw_knots, na.rm = TRUE),
      max_stw        = if (is.null(track_subset)) NA_real_ else max(track_subset$stw_knots, na.rm = TRUE),
      avg_tws        = if (is.null(track_subset)) NA_real_ else mean(track_subset$tws_knots, na.rm = TRUE),
      max_tws        = if (is.null(track_subset)) NA_real_ else max(track_subset$tws_knots, na.rm = TRUE),
      polar_perf_stw = if (is.null(track_subset)) NA_real_ else mean(track_subset$Polar_Perf_STW, na.rm = TRUE),
      polar_perf_sog = if (is.null(track_subset)) NA_real_ else mean(track_subset$Polar_Perf_SOG, na.rm = TRUE),
      duration_hrs   = if (is.null(track_subset)) NA_real_ else
        as.numeric(difftime(max(track_subset$datetime_local), min(track_subset$datetime_local), units = "hours"))
    ) |>
    ungroup() |>
    select(-track_subset, -cal_start, -cal_end) |>
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))

  completed <- all_races |>
    filter(!is.na(avg_sog), !is.nan(avg_sog))

  # Track used closing quotes across all races to avoid repeats
  narratives <- list()
  used_quotes <- character()
  for (i in seq_len(nrow(all_races))) {
    row <- all_races[i, ]
    key <- paste(row$race, row$race_date, sep = "|")
    result <- generate_race_narrative(row, completed, used_quotes)
    narratives[[key]] <- result$narrative
    if (!is.na(result$quote_used)) used_quotes <- c(used_quotes, result$quote_used)
  }
  narratives
}
