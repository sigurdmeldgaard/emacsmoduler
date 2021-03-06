;;; x-symbol-unicode-extras.el --- more Unicode tables for X-Symbol.
;;
;; Author: David Aspinall
;;
;;; Commentary:
;; 
;; Some new symbols possibly available in Unicode fonts
;;
;; TODO: need to define key short cuts, categories in a rational way
;;

;;; Code:
(defconst x-symol-unicode-extras
  '(
   (left-tortoise-shell-bracket "LEFT TORTOISE SHELL BRACKET")
   (right-tortoise-shell-bracket "RIGHT TORTOISE SHELL BRACKET")
   (left-white-tortoise-shell-bracket "LEFT WHITE TORTOISE SHELL BRACKET")
   (right-white-tortoise-shell-bracket "RIGHT WHITE TORTOISE SHELL BRACKET")
   (left-double-angle-bracket "LEFT DOUBLE ANGLE BRACKET")
   (right-double-angle-bracket "RIGHT DOUBLE ANGLE BRACKET")
   (left-black-lenticular-bracket "LEFT BLACK LENTICULAR BRACKET")
   (right-black-lenticular-bracket "RIGHT BLACK LENTICULAR BRACKET")
   (less-than-above-greater-than-above-double-line-equal
    "LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL")
   (greater-than-above-less-than-above-double-line-equal
    "GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL")
   (double-nested-less-than "DOUBLE NESTED LESS-THAN")
   (double-nested-greater-than "DOUBLE NESTED GREATER-THAN")
   (triangular-bullet "TRIANGULAR BULLET")
   (double-prime "DOUBLE PRIME")
   (triple-prime "TRIPLE PRIME")
   (reversed-prime "REVERSED PRIME")
   (reversed-double-prime "REVERSED DOUBLE PRIME")
   (reversed-triple-prime "REVERSED TRIPLE PRIME")
   (reference-mark "REFERENCE MARK")
   (caret-insertion-point "CARET INSERTION POINT")
   (asterism "ASTERISM")
   (close-up "CLOSE UP")
   (two-asterisks-aligned-vertically "TWO ASTERISKS ALIGNED VERTICALLY")
   (bet-symbol "BET SYMBOL")
   (gimel-symbol "GIMEL SYMBOL")
   (dalet-symbol "DALET SYMBOL")
   (information-source "INFORMATION SOURCE")
   (rotated-capital-q "ROTATED CAPITAL Q")
   (double-struck-small-gamma "DOUBLE-STRUCK SMALL GAMMA")
   (double-struck-capital-gamma "DOUBLE-STRUCK CAPITAL GAMMA")
   (double-struck-capital-pi "DOUBLE-STRUCK CAPITAL PI")
   (double-struck-n-ary-summation "DOUBLE-STRUCK N-ARY SUMMATION")
   (turned-sans-serif-capital-g "TURNED SANS-SERIF CAPITAL G")
   (turned-sans-serif-capital-l "TURNED SANS-SERIF CAPITAL L")
   (reversed-sans-serif-capital-l "REVERSED SANS-SERIF CAPITAL L")
   (turned-sans-serif-capital-y "TURNED SANS-SERIF CAPITAL Y")
   (double-struck-italic-capital-d "DOUBLE-STRUCK ITALIC CAPITAL D")
   (double-struck-italic-small-d "DOUBLE-STRUCK ITALIC SMALL D")
   (double-struck-italic-small-e "DOUBLE-STRUCK ITALIC SMALL E")
   (double-struck-italic-small-i "DOUBLE-STRUCK ITALIC SMALL I")
   (double-struck-italic-small-j "DOUBLE-STRUCK ITALIC SMALL J")
   (turned-ampersand "TURNED AMPERSAND")
   ;; More arrows
   (leftwards-arrow-with-stroke "LEFTWARDS ARROW WITH STROKE")
   (rightwards-arrow-with-stroke "RIGHTWARDS ARROW WITH STROKE")
   (leftwards-wave-arrow "LEFTWARDS WAVE ARROW")
   (rightwards-wave-arrow "RIGHTWARDS WAVE ARROW")
   (leftwards-two-headed-arrow "LEFTWARDS TWO HEADED ARROW")
   (upwards-two-headed-arrow "UPWARDS TWO HEADED ARROW")
   (rightwards-two-headed-arrow "RIGHTWARDS TWO HEADED ARROW")
   (downwards-two-headed-arrow "DOWNWARDS TWO HEADED ARROW")
   (leftwards-arrow-with-tail "LEFTWARDS ARROW WITH TAIL")
   (rightwards-arrow-with-tail "RIGHTWARDS ARROW WITH TAIL")
   (leftwards-arrow-from-bar "LEFTWARDS ARROW FROM BAR") ; maps from
   (up-down-arrow-with-base "UP DOWN ARROW WITH BASE")
   (leftwards-arrow-with-hook "LEFTWARDS ARROW WITH HOOK")
   (rightwards-arrow-with-hook "RIGHTWARDS ARROW WITH HOOK")
   (leftwards-arrow-with-loop "LEFTWARDS ARROW WITH LOOP")
   (rightwards-arrow-with-loop "RIGHTWARDS ARROW WITH LOOP")
   (left-right-wave-arrow "LEFT RIGHT WAVE ARROW")
   (left-right-arrow-with-stroke "LEFT RIGHT ARROW WITH STROKE")
   (downwards-zigzag-arrow "DOWNWARDS ZIGZAG ARROW")
   (upwards-arrow-with-tip-leftwards "UPWARDS ARROW WITH TIP LEFTWARDS")
   (upwards-arrow-with-tip-rightwards "UPWARDS ARROW WITH TIP RIGHTWARDS")
   (downwards-arrow-with-tip-leftwards "DOWNWARDS ARROW WITH TIP LEFTWARDS")
   (downwards-arrow-with-tip-rightwards "DOWNWARDS ARROW WITH TIP RIGHTWARDS")
   (rightwards-arrow-with-corner-downwards "RIGHTWARDS ARROW WITH CORNER DOWNWARDS")
   (downwards-arrow-with-corner-leftwards "DOWNWARDS ARROW WITH CORNER LEFTWARDS")
   (anticlockwise-top-semicircle-arrow "ANTICLOCKWISE TOP SEMICIRCLE ARROW")
   (clockwise-top-semicircle-arrow "CLOCKWISE TOP SEMICIRCLE ARROW")
   (north-west-arrow-to-long-bar "NORTH WEST ARROW TO LONG BAR")
   (leftwards-arrow-to-bar-over-rightwards-arrow-to-bar
    "LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR")
   ;;
   (rightwards-arrow-over-leftwards-arrow "RIGHTWARDS ARROW OVER LEFTWARDS ARROW")
   (upwards-arrow-leftwards-of-downwards-arrow "UPWARDS ARROW LEFTWARDS OF DOWNWARDS ARROW")
   (leftwards-arrow-over-rightwards-arrow "LEFTWARDS ARROW OVER RIGHTWARDS ARROW")
   (leftwards-paired-arrows "LEFTWARDS PAIRED ARROWS")
   (upwards-paired-arrows "UPWARDS PAIRED ARROWS")
   (rightwards-paired-arrows "RIGHTWARDS PAIRED ARROWS")
   (downwards-paired-arrows "DOWNWARDS PAIRED ARROWS")
   (leftwards-double-arrow-with-stroke "LEFTWARDS DOUBLE ARROW WITH STROKE")
   (left-right-double-arrow-with-stroke "LEFT RIGHT DOUBLE ARROW WITH STROKE")
   (rightwards-double-arrow-with-stroke "RIGHTWARDS DOUBLE ARROW WITH STROKE")
   (north-west-double-arrow "NORTH WEST DOUBLE ARROW")
   (north-east-double-arrow "NORTH EAST DOUBLE ARROW")
   (south-east-double-arrow "SOUTH EAST DOUBLE ARROW")
   (south-west-double-arrow "SOUTH WEST DOUBLE ARROW")
   (leftwards-triple-arrow "LEFTWARDS TRIPLE ARROW")
   (rightwards-triple-arrow "RIGHTWARDS TRIPLE ARROW")
   (upwards-arrow-with-double-stroke "UPWARDS ARROW WITH DOUBLE STROKE")
   (downwards-arrow-with-double-stroke "DOWNWARDS ARROW WITH DOUBLE STROKE")
   (leftwards-dashed-arrow "LEFTWARDS DASHED ARROW")
   (upwards-dashed-arrow "UPWARDS DASHED ARROW")
   (rightwards-dashed-arrow "RIGHTWARDS DASHED ARROW")
   (downwards-dashed-arrow "DOWNWARDS DASHED ARROW")
   (leftwards-arrow-to-bar "LEFTWARDS ARROW TO BAR")
   (rightwards-arrow-to-bar "RIGHTWARDS ARROW TO BAR")
   (leftwards-white-arrow "LEFTWARDS WHITE ARROW")
   (upwards-white-arrow "UPWARDS WHITE ARROW")
   (rightwards-white-arrow "RIGHTWARDS WHITE ARROW")
   (downwards-white-arrow "DOWNWARDS WHITE ARROW")
   (upwards-white-arrow-on-pedestal "UPWARDS WHITE ARROW ON PEDESTAL")
   (upwards-white-arrow-on-pedestal-with-horizontal-bar
    "UPWARDS WHITE ARROW ON PEDESTAL WITH HORIZONTAL BAR")
   (upwards-white-arrow-on-pedestal-with-vertical-bar
    "UPWARDS WHITE ARROW ON PEDESTAL WITH VERTICAL BAR")
   (upwards-white-double-arrow "UPWARDS WHITE DOUBLE ARROW")
   (upwards-white-double-arrow-on-pedestal "UPWARDS WHITE DOUBLE ARROW ON PEDESTAL")
   (rightwards-white-arrow-from-wall "RIGHTWARDS WHITE ARROW FROM WALL")
   (north-west-arrow-to-corner "NORTH WEST ARROW TO CORNER")
   (south-east-arrow-to-corner "SOUTH EAST ARROW TO CORNER")
   (up-down-white-arrow "UP DOWN WHITE ARROW")
   ;;;
   ;; More logical ops
   (there-does-not-exist "THERE DOES NOT EXIST")
   (small-element-of "SMALL ELEMENT OF")
   (contains-as-member "CONTAINS AS MEMBER")
   (does-not-contain-as-member "DOES NOT CONTAIN AS MEMBER")
   (small-contains-as-member "SMALL CONTAINS AS MEMBER")
   (end-of-proof "END OF PROOF")
   (measured-angle "MEASURED ANGLE")
   (spherical-angle "SPHERICAL ANGLE")
   (parallel-to "PARALLEL TO")
   (not-parallel-to "NOT PARALLEL TO")
   (double-integral "DOUBLE INTEGRAL")
   (triple-integral "TRIPLE INTEGRAL")
   (surface-integral "SURFACE INTEGRAL")
   (volume-integral "VOLUME INTEGRAL")
   (clockwise-integral "CLOCKWISE INTEGRAL")
   (clockwise-contour-integral "CLOCKWISE CONTOUR INTEGRAL")
   (anticlockwise-contour-integral "ANTICLOCKWISE CONTOUR INTEGRAL")
   (therefore "THEREFORE")
   (because "BECAUSE")
   (ratio "RATIO")
   (proportion "PROPORTION")
   (geometric-proportion "GEOMETRIC PROPORTION")
   (excess "EXCESS")
   ;; More equivalences
   (not-tilde "NOT TILDE")
   (minus-tilde "MINUS TILDE")
   (not-asymptotically-equal-to "NOT ASYMPTOTICALLY EQUAL TO")
   (approximately-but-not-actually-equal-to "APPROXIMATELY BUT NOT ACTUALLY EQUAL TO")
   (neither-approximately-nor-actually-equal-to
    "NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO")
   (almost-equal-to "ALMOST EQUAL TO")
   (not-almost-equal-to "NOT ALMOST EQUAL TO")
   (almost-equal-or-equal-to "ALMOST EQUAL OR EQUAL TO")
   (triple-tilde "TRIPLE TILDE")
   (all-equal-to "ALL EQUAL TO")
   (equivalent-to "EQUIVALENT TO")
   (geometrically-equivalent-to "GEOMETRICALLY EQUIVALENT TO")
   (difference-between "DIFFERENCE BETWEEN")
   (approaches-the-limit "APPROACHES THE LIMIT")
   (geometrically-equal-to "GEOMETRICALLY EQUAL TO")
   (approximately-equal-to-or-the-image-of "APPROXIMATELY EQUAL TO OR THE IMAGE OF")
   (image-of-or-approximately-equal-to "IMAGE OF OR APPROXIMATELY EQUAL TO")
   (colon-equals "COLON EQUALS")
   (equals-colon "EQUALS COLON")
   (ring-in-equal-to "RING IN EQUAL TO")
   (ring-equal-to "RING EQUAL TO")
   (corresponds-to "CORRESPONDS TO")
   (estimates "ESTIMATES")
   (equiangular-to "EQUIANGULAR TO")
   (star-equals "STAR EQUALS")
   (delta-equal-to "DELTA EQUAL TO")
   (equal-to-by-definition "EQUAL TO BY DEFINITION")
   (measured-by "MEASURED BY")
   (questioned-equal-to "QUESTIONED EQUAL TO")
   (not-equal-to "NOT EQUAL TO")
   ;; More comparisons
   (less-than-but-not-equal-to "LESS-THAN BUT NOT EQUAL TO")
   (greater-than-but-not-equal-to "GREATER-THAN BUT NOT EQUAL TO")
   (much-less-than "MUCH LESS-THAN")
   (much-greater-than "MUCH GREATER-THAN")
   (between "BETWEEN")
   (not-equivalent-to "NOT EQUIVALENT TO")
   (not-less-than "NOT LESS-THAN")
   (not-greater-than "NOT GREATER-THAN")
   (neither-less-than-nor-equal-to "NEITHER LESS-THAN NOR EQUAL TO")
   (neither-greater-than-nor-equal-to "NEITHER GREATER-THAN NOR EQUAL TO")
   (neither-less-than-nor-equivalent-to "NEITHER LESS-THAN NOR EQUIVALENT TO")
   (neither-greater-than-nor-equivalent-to "NEITHER GREATER-THAN NOR EQUIVALENT TO")
   (less-than-or-greater-than "LESS-THAN OR GREATER-THAN")
   (greater-than-or-less-than "GREATER-THAN OR LESS-THAN")
   (neither-less-than-nor-greater-than "NEITHER LESS-THAN NOR GREATER-THAN")
   (neither-greater-than-nor-less-than "NEITHER GREATER-THAN NOR LESS-THAN")
   (does-not-precede "DOES NOT PRECEDE")
   (does-not-succeed "DOES NOT SUCCEED")
   (not-a-subset-of "NOT A SUBSET OF")
   (not-a-superset-of "NOT A SUPERSET OF")
   (neither-a-subset-of-nor-equal-to "NEITHER A SUBSET OF NOR EQUAL TO")
   (neither-a-superset-of-nor-equal-to "NEITHER A SUPERSET OF NOR EQUAL TO")
   (subset-of-with-not-equal-to "SUBSET OF WITH NOT EQUAL TO")
   (superset-of-with-not-equal-to "SUPERSET OF WITH NOT EQUAL TO")
   ;; More ops
   (squared-plus "SQUARED PLUS")
   (squared-minus "SQUARED MINUS")
   (squared-times "SQUARED TIMES")
   (squared-dot-operator "SQUARED DOT OPERATOR")
   ;; More rels
   (forces "FORCES")
   (triple-vertical-bar-right-turnstile "TRIPLE VERTICAL BAR RIGHT TURNSTILE")
   (double-vertical-bar-double-right-turnstile "DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE")
   (does-not-prove "DOES NOT PROVE")
   (not-true "NOT TRUE")
   (does-not-force "DOES NOT FORCE")
   (negated-double-vertical-bar-double-right-turnstile
    "NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE")
   (precedes-under-relation "PRECEDES UNDER RELATION")
   (succeeds-under-relation "SUCCEEDS UNDER RELATION")
   (original-of "ORIGINAL OF")
   (image-of "IMAGE OF")
   (xor "XOR")
   (nand "NAND")
   (nor "NOR")
   (division-times "DIVISION TIMES")
   (curly-logical-or "CURLY LOGICAL OR")
   (curly-logical-and "CURLY LOGICAL AND")
   (double-subset "DOUBLE SUBSET")
   (double-superset "DOUBLE SUPERSET")
   (double-intersection "DOUBLE INTERSECTION")
   (double-union "DOUBLE UNION")
   (pitchfork "PITCHFORK")
   (equal-and-parallel-to "EQUAL AND PARALLEL TO")
   (less-than-with-dot "LESS-THAN WITH DOT")
   (greater-than-with-dot "GREATER-THAN WITH DOT")
   (very-much-less-than "VERY MUCH LESS-THAN")
   (very-much-greater-than "VERY MUCH GREATER-THAN")
   (less-than-equal-to-or-greater-than "LESS-THAN EQUAL TO OR GREATER-THAN")
   (greater-than-equal-to-or-less-than "GREATER-THAN EQUAL TO OR LESS-THAN")
   (equal-to-or-less-than "EQUAL TO OR LESS-THAN")
   (equal-to-or-greater-than "EQUAL TO OR GREATER-THAN")
   (equal-to-or-precedes "EQUAL TO OR PRECEDES")
   (equal-to-or-succeeds "EQUAL TO OR SUCCEEDS")
   (does-not-precede-or-equal "DOES NOT PRECEDE OR EQUAL")
   (does-not-succeed-or-equal "DOES NOT SUCCEED OR EQUAL")
   (equal-to-or-greater-than "EQUAL TO OR GREATER-THAN")
   (equal-to-or-precedes "EQUAL TO OR PRECEDES")
   (equal-to-or-succeeds "EQUAL TO OR SUCCEEDS")
   (does-not-precede-or-equal "DOES NOT PRECEDE OR EQUAL")
   (does-not-succeed-or-equal "DOES NOT SUCCEED OR EQUAL")
   (not-square-image-of-or-equal-to "NOT SQUARE IMAGE OF OR EQUAL TO")
   (not-square-original-of-or-equal-to "NOT SQUARE ORIGINAL OF OR EQUAL TO")
   (square-image-of-or-not-equal-to "SQUARE IMAGE OF OR NOT EQUAL TO")
   (square-original-of-or-not-equal-to "SQUARE ORIGINAL OF OR NOT EQUAL TO")
   (less-than-but-not-equivalent-to "LESS-THAN BUT NOT EQUIVALENT TO")
   (greater-than-but-not-equivalent-to "GREATER-THAN BUT NOT EQUIVALENT TO")
   (precedes-but-not-equivalent-to "PRECEDES BUT NOT EQUIVALENT TO")
   (succeeds-but-not-equivalent-to "SUCCEEDS BUT NOT EQUIVALENT TO")
   (not-normal-subgroup-of "NOT NORMAL SUBGROUP OF")
   (does-not-contain-as-normal-subgroup "DOES NOT CONTAIN AS NORMAL SUBGROUP")
   (not-normal-subgroup-of-or-equal-to "NOT NORMAL SUBGROUP OF OR EQUAL TO")
   (does-not-contain-as-normal-subgroup-or-equal
    "DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL")
   ;; misc syms
   (watch "WATCH")
   (hourglass "HOURGLASS")
   (option-key "OPTION KEY")
   (erase-to-the-right "ERASE TO THE RIGHT")
   (erase-to-the-left "ERASE TO THE LEFT")
   (broken-circle-with-northwest-arrow "BROKEN CIRCLE WITH NORTHWEST ARROW")
   (helm-symbol "HELM SYMBOL")
   ;; dingbats
   (cloud "CLOUD")
   (umbrella "UMBRELLA")
   (snowman "SNOWMAN")
   (comet "COMET")
   (black-star "BLACK STAR")
   (white-star "WHITE STAR")
   (lightning "LIGHTNING")
   (thunderstorm "THUNDERSTORM")
   (ascending-node "ASCENDING NODE")
   (descending-node "DESCENDING NODE")
   (conjunction "CONJUNCTION")
   (opposition "OPPOSITION")
   (left-half-black-circle "LEFT HALF BLACK CIRCLE")
   (right-half-black-circle "RIGHT HALF BLACK CIRCLE")
   (square-with-left-half-black "SQUARE WITH LEFT HALF BLACK")
   (square-with-right-half-black "SQUARE WITH RIGHT HALF BLACK")
   (square-with-upper-left-diagonal-half-black "SQUARE WITH UPPER LEFT DIAGONAL HALF BLACK")
   (square-with-lower-right-diagonal-half-black "SQUARE WITH LOWER RIGHT DIAGONAL HALF BLACK")
   (up-pointing-triangle-with-left-half-black "UP-POINTING TRIANGLE WITH LEFT HALF BLACK")
   (up-pointing-triangle-with-right-half-black "UP-POINTING TRIANGLE WITH RIGHT HALF BLACK")
   (black-sun-with-rays "BLACK SUN WITH RAYS")
   (white-shogi-piece "WHITE SHOGI PIECE")
   (black-shogi-piece "BLACK SHOGI PIECE")
   (white-left-pointing-index "WHITE LEFT POINTING INDEX")
   (white-up-pointing-index "WHITE UP POINTING INDEX")
   (white-right-pointing-index "WHITE RIGHT POINTING INDEX")
   (white-down-pointing-index "WHITE DOWN POINTING INDEX")
   (skull-and-crossbones "SKULL AND CROSSBONES")
   (caution-sign "CAUTION SIGN")
   (radioactive-sign "RADIOACTIVE SIGN")
   (biohazard-sign "BIOHAZARD SIGN")
   (caduceus "CADUCEUS")
   (ankh "ANKH")
   (orthodox-cross "ORTHODOX CROSS")
   (chi-rho "CHI RHO")
   (cross-of-lorraine "CROSS OF LORRAINE")
   (cross-of-jerusalem "CROSS OF JERUSALEM")
   (star-and-crescent "STAR AND CRESCENT")
   (farsi-symbol "FARSI SYMBOL")
   (adi-shakti "ADI SHAKTI")
   (hammer-and-sickle "HAMMER AND SICKLE")
   (peace-symbol "PEACE SYMBOL")
   (yin-yang "YIN YANG")
   (wheel-of-dharma "WHEEL OF DHARMA")
   (white-frowning-face "WHITE FROWNING FACE")
   (white-smiling-face "WHITE SMILING FACE")
   (black-smiling-face "BLACK SMILING FACE")
   (white-sun-with-rays "WHITE SUN WITH RAYS")
   (first-quarter-moon "FIRST QUARTER MOON")
   (last-quarter-moon "LAST QUARTER MOON")
   (mercury "MERCURY")
   (female-sign "FEMALE SIGN")
   (earth "EARTH")
   (male-sign "MALE SIGN")
   (jupiter "JUPITER")
   (saturn "SATURN")
   (uranus "URANUS")
   (neptune "NEPTUNE")
   (pluto "PLUTO")
   (aries "ARIES")
   (taurus "TAURUS")
   (gemini "GEMINI")
   (cancer "CANCER")
   (leo "LEO")
   (virgo "VIRGO")
   (libra "LIBRA")
   (scorpius "SCORPIUS")
   (sagittarius "SAGITTARIUS")
   (capricorn "CAPRICORN")
   (aquarius "AQUARIUS")
   (pisces "PISCES")
   ;; theorem provers like to play chess
   (white-chess-king "WHITE CHESS KING")
   (white-chess-queen "WHITE CHESS QUEEN")
   (white-chess-rook "WHITE CHESS ROOK")
   (white-chess-bishop "WHITE CHESS BISHOP")
   (white-chess-knight "WHITE CHESS KNIGHT")
   (white-chess-pawn "WHITE CHESS PAWN")
   (black-chess-king "BLACK CHESS KING")
   (black-chess-queen "BLACK CHESS QUEEN")
   (black-chess-rook "BLACK CHESS ROOK")
   (black-chess-bishop "BLACK CHESS BISHOP")
   (black-chess-knight "BLACK CHESS KNIGHT")
   (black-chess-pawn "BLACK CHESS PAWN")
   ;; or cards
   (black-spade-suit "BLACK SPADE SUIT")
   (white-heart-suit "WHITE HEART SUIT")
   (white-diamond-suit "WHITE DIAMOND SUIT")
   (black-club-suit "BLACK CLUB SUIT")
   (white-spade-suit "WHITE SPADE SUIT")
   (black-heart-suit "BLACK HEART SUIT")
   (black-diamond-suit "BLACK DIAMOND SUIT")
   (white-club-suit "WHITE CLUB SUIT")
   (hot-springs "HOT SPRINGS")
   ;; or music?
   (quarter-note "QUARTER NOTE")
   (eighth-note "EIGHTH NOTE")
   (beamed-eighth-notes "BEAMED EIGHTH NOTES")
   (beamed-sixteenth-notes "BEAMED SIXTEENTH NOTES")
   (universal-recycling-symbol "UNIVERSAL RECYCLING SYMBOL")
   (west-syriac-cross "WEST SYRIAC CROSS")
   (east-syriac-cross "EAST SYRIAC CROSS")
   (upper-blade-scissors "UPPER BLADE SCISSORS")
   (black-scissors "BLACK SCISSORS")
   (check-mark "CHECK MARK")
   (heavy-check-mark "HEAVY CHECK MARK")
   (multiplication-x "MULTIPLICATION X")
   (heavy-multiplication-x "HEAVY MULTIPLICATION X")
   (ballot-x "BALLOT X")
   (heavy-ballot-x "HEAVY BALLOT X")
   (outlined-greek-cross "OUTLINED GREEK CROSS")
   (heavy-greek-cross "HEAVY GREEK CROSS")
   (open-centre-cross "OPEN CENTRE CROSS")
   (heavy-open-centre-cross "HEAVY OPEN CENTRE CROSS")
   (latin-cross "LATIN CROSS")
   (shadowed-white-latin-cross "SHADOWED WHITE LATIN CROSS")
   (outlined-latin-cross "OUTLINED LATIN CROSS")
   (maltese-cross "MALTESE CROSS")
   (star-of-david "STAR OF DAVID")
   (four-teardrop-spoked-asterisk "FOUR TEARDROP-SPOKED ASTERISK")
   (four-balloon-spoked-asterisk "FOUR BALLOON-SPOKED ASTERISK")
   (heavy-four-balloon-spoked-asterisk "HEAVY FOUR BALLOON-SPOKED ASTERISK")
   (four-club-spoked-asterisk "FOUR CLUB-SPOKED ASTERISK")
   (black-four-pointed-star "BLACK FOUR POINTED STAR")
   (white-four-pointed-star "WHITE FOUR POINTED STAR")
   (stress-outlined-white-star "STRESS OUTLINED WHITE STAR")
   (circled-white-star "CIRCLED WHITE STAR")
   (open-centre-black-star "OPEN CENTRE BLACK STAR")
   (black-centre-white-star "BLACK CENTRE WHITE STAR")
   (outlined-black-star "OUTLINED BLACK STAR")
   (medium-left-pointing-angle-bracket-ornament
    "MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT")
   (medium-right-pointing-angle-bracket-ornament
    "MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT")
   (heavy-left-pointing-angle-quotation-mark-ornament
    "HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT")
   (heavy-right-pointing-angle-quotation-mark-ornament
    "HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT")
   (heavy-left-pointing-angle-bracket-ornament
    "HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT")
   (heavy-right-pointing-angle-bracket-ornament
    "HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT")
   (light-left-tortoise-shell-bracket-ornament
    "LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT")
   (light-right-tortoise-shell-bracket-ornament
    "LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT")
   (medium-left-curly-bracket-ornament "MEDIUM LEFT CURLY BRACKET ORNAMENT")
   (medium-right-curly-bracket-ornament "MEDIUM RIGHT CURLY BRACKET ORNAMENT")
   (heavy-wide-headed-rightwards-arrow "HEAVY WIDE-HEADED RIGHTWARDS ARROW")
   (heavy-south-east-arrow "HEAVY SOUTH EAST ARROW")
   (heavy-rightwards-arrow "HEAVY RIGHTWARDS ARROW")
   (heavy-north-east-arrow "HEAVY NORTH EAST ARROW")
   (drafting-point-rightwards-arrow "DRAFTING POINT RIGHTWARDS ARROW")
   (heavy-round-tipped-rightwards-arrow "HEAVY ROUND-TIPPED RIGHTWARDS ARROW")
   (triangle-headed-rightwards-arrow "TRIANGLE-HEADED RIGHTWARDS ARROW")
   (heavy-triangle-headed-rightwards-arrow "HEAVY TRIANGLE-HEADED RIGHTWARDS ARROW")
   (dashed-triangle-headed-rightwards-arrow "DASHED TRIANGLE-HEADED RIGHTWARDS ARROW")
   (heavy-dashed-triangle-headed-rightwards-arrow
    "HEAVY DASHED TRIANGLE-HEADED RIGHTWARDS ARROW")
   (black-rightwards-arrow "BLACK RIGHTWARDS ARROW")
   (three-d-top-lighted-rightwards-arrowhead "THREE-D TOP-LIGHTED RIGHTWARDS ARROWHEAD")
   (three-d-bottom-lighted-rightwards-arrowhead
    "THREE-D BOTTOM-LIGHTED RIGHTWARDS ARROWHEAD")
   (leftwards-harpoon-with-barb-up-above-leftwards-harpoon-with-barb-down
    "LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN")
   (upwards-harpoon-with-barb-left-beside-upwards-harpoon-with-barb-right
    "UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT")
   (rightwards-harpoon-with-barb-up-above-rightwards-harpoon-with-barb-down
    "RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN")
   (downwards-harpoon-with-barb-left-beside-downwards-harpoon-with-barb-right
    "DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT")
   (left-triangle-beside-vertical-bar "LEFT TRIANGLE BESIDE VERTICAL BAR")
   (vertical-bar-beside-right-triangle "VERTICAL BAR BESIDE RIGHT TRIANGLE")
   (double-plus "DOUBLE PLUS")
   (triple-plus "TRIPLE PLUS")
   (short-up-tack-with-underbar "SHORT UP TACK WITH UNDERBAR")
   ))


(provide 'x-symbol-unicode-extras)

;;; x-symbol-unicode-extras.el ends here
