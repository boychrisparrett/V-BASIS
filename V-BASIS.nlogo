;;***************************************************************************
;; Author: Christopher M. Parrett, PhD(c)
;; Email:  cparret2@gmu.edu
;;         College of Computational and Data Sciences
;;         George Mason University, November 2019
;; Developed on a Windows 10 platform, Intel(R) i7-6500U 2.5GHz w/ 8 GB RAM
;;    using NetLogo v6.1.1 - Copyright 1999-2019 by Uri Wilensky.
;;    http://ccl.northwestern.edu/netlogo/
;;***************************************************************************

;;################################################################################
;; Define Internal Model Parameters, mostly for readability
globals [
  G_HEALTHY
  G_ASYMPTOM
  G_SYMPTOM
  G_SICK
  G_RECOVERED
  G_DEAD
]

;;################################################################################
;; Define Quarantine "Shelters" - think of these as hospitals
breed [shelters shelter]
shelters-own [
  status        ;;Open / Closed
  shelterrad    ;;radius of regard, in pixels - NOT IMPLEMENTED
  capacity      ;;Maximum capacity
]

;;################################################################################
;; Define Primary Agents - People/Person
breed [people person]
people-own [
   orig_health    ;;Starting health of individual
   health         ;;Current health of patient
   infected?      ;;Has this agent been infected?
   exposed?       ;;Has this agent been exposed to the virus?
   status         ;;What is the current state of the agent (in Globals)
   statchange     ;;Time of last status update (e.g. - time from Susceptible to Exposed
   recovered?     ;;Has the agent recoverd... assuming immune
   quarantined?   ;;Is this agent current under quarantine?
]

patches-own [
  density
  infectrate
]

;;################################################################################
;; Setup the model, reseting first; redefine globals and instantiate populations
to setup
  clear-all
  reset-ticks

  ;; Setup Gloabl Variables for Readability
  set G_HEALTHY 0
  set G_ASYMPTOM 1
  set G_SYMPTOM 2
  set G_SICK 3
  set G_RECOVERED 4
  set G_DEAD 5

  ;; setup patches
  ask patches [
    set infectrate 0
  ]
  ;; Establish initial population
  create-people population [
    ;;Turtle properties
    setxy random-xcor random-ycor
    set color blue
    set shape "Circle"
    set size 0.65

    ;; People properties
    set status G_HEALTHY
    set statchange 0
    set exposed? False
    set infected? False
    set recovered? False
    set quarantined? False

    ;; For now, health is a normal distribution
    set health random-normal (HealthIndex / 100) STDev
    set orig_health health
    ;; Establish initial infected population
    if (random 100) < InitInfectedPop [
      infect
    ]
  ]
  if count people with [infected?] = 0[
    ask one-of people [
      infect
    ]
  ]

  ;; Create Shelters
  create-shelters NumShelters [
    setxy random-xcor random-ycor
    set shape "Circle 2"
    set color white
    set size 5
    set status 1
    set shelterrad 1
    set capacity ShelterCapacity
  ]

end

;;################################################################################
;;
to go
  Walk
  Interact
  Quarantine
  Update
  tick
end

;;################################################################################
;;
to Walk
  ;; Move all people... except the dead ones... that is a different model
  ask people with [(status != G_DEAD) and not quarantined?] [

    let crrnt_max_head (random 360)
    let crrnt_max_dist 0.0

    ifelse SocialDistance > 0.0 [
      ;; If Social Distancing, look around Vision and move away
      if ((count people in-radius Vision) > 0) [
        ;; More than one turtle on patch move toward less dense area
        ask patches in-radius Vision [
          ask other people-here  [
            ;;for each patch, check the number of
            if ((distance myself) / 2.0) > crrnt_max_dist [
              set crrnt_max_dist ((distance myself) / 2.0)
              set crrnt_max_head towards myself
            ]
          ]
        ]

        set heading crrnt_max_head
        fd crrnt_max_dist
      ]
    ][
      ;; NO SocialDistance
      ;; walk
      fd 1
    ]
  ]
end

;;################################################################################
;;
to Interact
  ask people with [not quarantined?] [
    if infected? [
      (ifelse
        status = G_ASYMPTOM and (ASYM_Spread? or Asymptomatic = 0) [
          ask people-here with [status = G_HEALTHY] [
            ifelse (random 100) <= Prob_of_Infection [
              infect
            ][
              set exposed? True
              set shape "Circle"
            ]
          ]
        ]
        status = G_SICK [
          ask people-here with [status = G_HEALTHY] [
            ifelse (random 100) <= Prob_of_Infection [
              infect
            ][
              set exposed? True
              set shape "Circle"
            ]
          ]
        ]
        ;;else
        [
      ])
    ]
  ]
end

;;################################################################################
;;
to infect
  set status G_ASYMPTOM
  set infected? True
  set exposed? True
  set color red
  set shape "Triangle"
  set size 1
  set statchange ticks
  ask patch-here [
     set infectrate  infectrate + 1
  ]
end

;;################################################################################
;;
to Quarantine
  ask shelters [
    set capacity (count people-here with [quarantined?])
    let pctcap ((capacity / ShelterCapacity)* 100)
    show (word "Shelter " self " at " pctcap "%")
    ifelse capacity >= ShelterCapacity [
      set color red
      ;;What to do?
      print (word "Shelter Capacity Exceeded. Operating at " ((capacity / ShelterCapacity)* 100) "%")
    ][
      let sickpop (count people with [status = G_SICK and not quarantined?])
      if sickpop > 0 [
        let mypop min list (ShelterCapacity - capacity) sickpop
        ask n-of mypop people with [status = G_SICK and not quarantined?] [
          move-to myself
          set quarantined? true
        ]
      ]
    ]
  ]
end

;;################################################################################
;;
to update
  ;; Step through all people's status for a health status update
  ask people with [infected?] [
    (ifelse
      status = G_ASYMPTOM [
        ;; Asymptomatic... no change in health.
        if (ticks - statchange) >  (random Asymptomatic) [
          set status G_SYMPTOM
          set statchange ticks
        ]
      ]
      status = G_SYMPTOM [
        ;; Symptomatic/sick... declining health
        set health (health * 0.95)
        if ((ticks - statchange) >  (random Sick)) [
          ifelse health < Sick_Thresh [
            set status G_SICK
            set statchange ticks
          ] [
            Recover
          ]
        ]
      ]
      status = G_SICK [
        ;; Seriously sick... rapidly declining health; seek medical
        ifelse quarantined? [
          ;;Admitted to a hospital
          set health (health * 0.90)
        ][
          ;;No Admittance
          set health (health * (1.0 - Mortality * (1 / Sick)))
        ]

        ;;If health falls below a threshold, person succumbs
        if health < M_Thresh [
          Succumb
        ]

        ;;Chance for pre-existing condition, person succumbs
        if (random-float 1.0) < Mortality [
          Succumb
        ]

        ;;Enough time has elapsed that a healthy indivdual emerges
        if ((ticks - statchange) >  (random Sick)) [
          Recover
        ]
      ]
      [ ;else
      ]
      )
  ]
  ask patches [
    set pcolor scale-color red infectrate 0 50
  ]
end

to Recover
  set status G_RECOVERED
  set statchange ticks
  set color green
  set shape "Square"
  set recovered? True
end

to Succumb
  set status G_DEAD
  set recovered? False
  set statchange ticks
  set color gray
  set shape "X"
end
@#$#@#$#@
GRAPHICS-WINDOW
268
15
882
630
-1
-1
6.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
0
0
1
ticks
30.0

SLIDER
10
133
127
166
Asymptomatic
Asymptomatic
1
30
14.0
1
1
NIL
HORIZONTAL

SLIDER
129
132
262
165
Sick
Sick
0
30
7.0
1
1
NIL
HORIZONTAL

SLIDER
10
172
263
205
InitInfectedPop
InitInfectedPop
0
100
1.0
1
1
%
HORIZONTAL

SLIDER
10
96
126
129
Mortality
Mortality
0
100
5.0
1
1
%
HORIZONTAL

SLIDER
129
96
263
129
Prob_of_Infection
Prob_of_Infection
0
100
70.0
1
1
%
HORIZONTAL

SLIDER
10
256
195
289
Population
Population
10
10200
10000.0
10
1
NIL
HORIZONTAL

BUTTON
13
26
77
59
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
85
27
143
60
Go
Go\nif count people with [status > 0 and status < 4] = 0 [\n   stop\n]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
901
15
1343
289
Population Status
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Asymptomatic" 1.0 0 -2064490 true "" "plot count people with [status = 1]"
"Sick" 1.0 0 -2674135 true "" "plot (count people with [status = 2] + count people with [status = 3])"

SLIDER
9
445
119
478
SocialDistance
SocialDistance
0
5
1.0
0.1
1
NIL
HORIZONTAL

MONITOR
901
289
978
334
Non-Exposed
(count turtles with [status = 0]) / (count turtles)
4
1
11

MONITOR
1061
289
1132
334
Asymptom
(count turtles with [status = 1]) / (count turtles)
4
1
11

MONITOR
1142
289
1192
334
Sick
(count turtles with [status = 2] + count turtles with [status = 3]) / (count turtles)
4
1
11

MONITOR
1203
289
1276
334
Recovered
(count turtles with [status = 4]) / (count turtles)
4
1
11

MONITOR
1289
289
1343
334
Dead
(count turtles with [status = 5]) / (count turtles)
4
1
11

SLIDER
10
294
197
327
HealthIndex
HealthIndex
1
100
85.0
1
1
%
HORIZONTAL

INPUTBOX
202
304
263
364
STDev
0.1
1
0
Number

SLIDER
9
405
118
438
NumShelters
NumShelters
0
10
0.0
1
1
NIL
HORIZONTAL

MONITOR
202
258
263
303
Density
(count people with [status != 5]) / (count patches)
4
1
11

MONITOR
900
342
977
387
Quarantine
(count people with [quarantined?]) / (count people)
4
1
11

SLIDER
121
405
247
438
ShelterCapacity
ShelterCapacity
10
1000
200.0
10
1
NIL
HORIZONTAL

BUTTON
152
27
215
60
Step
Go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
981
289
1054
334
Exposed
count people with [exposed?] / (count people)
4
1
11

PLOT
903
393
1342
633
Health Index
NIL
NIL
0.0
1.0
0.0
10.0
true
true
"" ""
PENS
"Orig  Health Index" 0.05 1 -16777216 true "" "histogram [orig_health] of people"
"Current Health Index" 0.05 1 -8053223 true "" "histogram [health] of people"

SWITCH
11
213
150
246
ASYM_Spread?
ASYM_Spread?
0
1
-1000

INPUTBOX
91
338
161
398
M_Thresh
0.2
1
0
Number

INPUTBOX
9
338
80
398
Sick_Thresh
0.7
1
0
Number

SLIDER
121
446
246
479
Vision
Vision
1
5
2.0
1
1
NIL
HORIZONTAL

BUTTON
9
489
90
522
Hide Turtles
ask turtles [\n   set hidden? not hidden?\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

COVID Simulator

## HOW IT WORKS

  * The model starts with an initial **POPULATION** and an initial infected population controlled by **InitInfectedPop**.

  * The virus has a **Mortality** rate and a probability of infection **ProbOfInfection**.

  * The virus spreads through contact, which here is defined as two or more turtles occupying the same patch.

  * The virus first infects the person, who then becomes asymptomatic. COVID-19 has shown that the viral load of an asymptomatic person is greater than that of a symptomatic person. 

  * The virus then transitions to symptomatic, at which point the person exhbits the sickness and begins to lose health at a predetermined rate.

  * The virus then transitions to sick if the person's health drops below 50%. In this stage, health degrades twice as fast, and if sick too long, dies. Also, the person risks death at the Mortality rate, simulating pre-existing conditions. The dead do not walk in this model (Would make for a great TV show though!). 

  * If the person lasts through the the aymptomatic->sick process with enough health, they are marked Cured and immune to the disease.

  * There is a **SocialDistance** control which each person tries to adhere to by keeping that number of patches between themselves and other people.

  * There are shelters that are randomly placed. When present, all turtles move toward the nearest shleter at 1 patch per tick. Once at a shelter, the person is marked as in quarantine and their health status changes only if already infected.


  * The People agents are coded as:
    * Blue Circles: Non-Exposed
    * Red Triangles: Exposed/Sick
    * Green Squares: Cured
    * Grey X's: Dead


## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Asymptom_Sweep" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go
if count people with [status &gt; 0 and status &lt; 4] = 0 [
   stop
]</go>
    <metric>(count people with [status = 0])</metric>
    <metric>(count people with [status = 1])</metric>
    <metric>(count people with [status = 2])</metric>
    <metric>(count people with [status = 3])</metric>
    <metric>(count people with [status = 4])</metric>
    <metric>(count people with [status = 5])</metric>
    <metric>(count people with [exposed?])</metric>
    <metric>(count people with [infected?])</metric>
    <metric>(count people with [recovered?])</metric>
    <metric>(count people with [quarantined?])</metric>
    <enumeratedValueSet variable="STDev">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HealthIndex">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumShelters">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SocialDistance">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Asymptomatic" first="1" step="2" last="22"/>
    <enumeratedValueSet variable="Prob_of_Infection">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population">
      <value value="5000"/>
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ASYM_Spread?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InitInfectedPop">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mortality">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShelterCapacity">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sick">
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Quick" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>Go
if count people with [status &gt; 0 and status &lt; 4] = 0 [
   stop
]</go>
    <metric>count people with [status = 0]</metric>
    <metric>count people with [status = 1]</metric>
    <metric>count people with [status = 2]</metric>
    <metric>count people with [status = 3]</metric>
    <enumeratedValueSet variable="HealthIndex">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumShelters">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="M_Thresh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SocialDistance">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ASYM_Spread?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mortality">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sick">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="STDev">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sick_Thresh">
      <value value="0.7"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Asymptomatic" first="1" step="1" last="14"/>
    <enumeratedValueSet variable="Prob_of_Infection">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population">
      <value value="8000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InitInfectedPop">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShelterCapacity">
      <value value="1000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SD_SWEEP" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go
if count people with [status &gt; 0 and status &lt; 4] = 0 [
   stop
]</go>
    <metric>(count people with [status = 0])</metric>
    <metric>(count people with [status = 1])</metric>
    <metric>(count people with [status = 2])</metric>
    <metric>(count people with [status = 3])</metric>
    <metric>(count people with [status = 4])</metric>
    <metric>(count people with [status = 5])</metric>
    <metric>(count people with [exposed?])</metric>
    <metric>(count people with [infected?])</metric>
    <metric>(count people with [recovered?])</metric>
    <metric>(count people with [quarantined?])</metric>
    <enumeratedValueSet variable="STDev">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HealthIndex">
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NumShelters">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SocialDistance">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Asymptomatic">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prob_of_Infection">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ASYM_Spread?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="InitInfectedPop">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mortality">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ShelterCapacity">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sick">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
