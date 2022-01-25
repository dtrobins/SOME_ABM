extensions [ fetch import-a ]

breed [ residents resident ]  ;; create residents breed
breed [ services service ]    ;; create service breed
breed [ circles circle ]      ;; create service shade

residents-own [rnum cluster alphaq alphas alphac utility]                         ;; residents' variables
patches-own [quality sddist comfort ltype]                                        ;; landscape variables
globals [counter selx sely residentsperstep residentsperservice]  ;; global variables



;; THIS IS THE SETUP STEP
;;----- Calls processes that set up the initial service center and the patches.

to SETUP

  clear-all

  ;; set up the initial variable values
  reset-ticks
  setup-service
  ;; setup-patches
  import-world-forWeb

  set residentsperstep 10
  set residentsperservice 100
  set-default-shape circles "box"

end

to import-world-forWeb
  ;;let world-file "630_SOME_model world.csv"

  ;;fetch:user-file-async [
  ;;  text ->
  ;;    import-a:world text
  ;;    show "Success!"
  ;;]

  fetch:url-async "https://raw.githubusercontent.com/dtrobins/SOME_ABM/main/630_SOME_model%20world.csv" [
    text ->
      import-a:world text
      show "Success!"
  ]
  
end

;; This is the to go step
to go
  locate-resident   ;; locate residents
  ;; if the number of residents in a service center's service shade, locate another service center
  if counter > residentsperservice
    [
      locate-service
      set counter 0
    ]
  ;; if the total number of residents is over 3420, stop the model
  if count residents >= 3420
  [

    stop
  ]

  tick

end

;; set up the initial variable values when doing the iteration
to initial-clearup
  clear-all-plots
  clear-drawing
  clear-links
  clear-output
  clear-patches
  clear-plot
  clear-ticks
  clear-turtles
  set counter 0
  set selx 0
  set sely 0
  set residentsperstep 0
  set residentsperservice 0
end


;;----- Create initial service center in the middle
to setup-service
    create-services 1
    ask services
        [
        set color red
        set shape "box"
        ]
end

;;----- Assign and scale initial values of aesthetic quality and distance to services
to setup-patches
  ;; Create an empty list to store landscape quality values acquired from the txt file
  let patch-qualities []

  ;; Store the landscape quality value from txt file to the empty list
  ;; file-open "151aesth.txt"
  ;; while [not file-at-end?]
  ;; [
    ;; set patch-qualities lput file-read patch-qualities
  ;; ]
  ;; file-close

  ;; Populating the list
  (foreach sort patches patch-qualities [ [the-patch the-quality] ->
    ask the-patch [set quality (the-quality / 8191)]
  ])

  ask patches [set ltype 0]

  repeat smoothness [diffuse quality 1]
  ;; set up the service center's service shade
  ask patches
  [
    set sddist min [distance myself] of services
    set pcolor scale-color green quality (0)(1)
    if (distancexy 0 0) < radius + 0.5 and (distancexy 0 0) > radius - 0.5
    [set pcolor yellow]
  ]
end



;; THIS SECTION LOCATES RESIDENTS
;;----- This counts incoming residents, calls a routine to locate them, than upates aesthetic quality
to locate-resident
        repeat residentsperstep
    [
        create-residents 1
        set counter ( counter + 1 )
        ask max-one-of residents [who]
            [
            set color 114  ;;; violet
            set shape "box"

            set-alphas              ;;; determine preferences
            evaluate                ;;; select location
            ask patch-here [set ltype 1]
            if ( luab? = true )     ;;; reduce aesthetic quality if luab? is switched on
                 [
                 ask patch-here [ set quality ( quality * 0.75 ) ]
                 ask neighbors [ set quality ( quality * 0.9 ) ]
                 ask neighbors4 [ set quality ( quality * 0.9 ) ]
                 set pcolor scale-color green quality ( 0.1 * ( max-pxcor * 1.414 ) ) ( 0.9 * ( max-pxcor * 1.414 ) )
                 ask neighbors
                     [
                     set pcolor scale-color green quality
                     ( 0.1 * ( max-pxcor * 1.414 ) )
                     ( 0.9 * ( max-pxcor * 1.414 ) )
                     ]
                 ]
            ]
    ]
end

;;----- Set alpha values, alternative procedures could be used here (based on different experiment settings)
to set-alphas
  set alphas (random-normal 0.536 0.244)
  while [alphas < 0 or alphas > 1] [set alphas (random-normal 0.536 0.244)]
  set alphaq (random-normal 0.532 0.261)
  while [alphaq < 0 or alphaq > 1] [set alphaq (random-normal 0.532 0.261)]
  set alphac (random-normal 0.494 0.248) ;; control_file: alphaT = normal,0.494,0.248
  while [alphac < 0 or alphac > 1] [set alphac (random-normal 0.494 0.248)]

  let total_alpha (alphas + alphaq + alphac)
  set alphas (alphas / total_alpha)
  set alphaq (alphaq / total_alpha)
  set alphac (alphac / total_alpha)
end

;; Set the neighborhood similarity values
to set-comfort [canx cany]
  let n 8
  let total 0
  ask patch canx cany[set comfort 0.5]

  let ix -1
  let iy -1

  while [ix < 2] [
    while [iy < 2] [
      ;; exclude the center patch
      if (ix = 0) and (iy = 0) [
        set iy iy + 1
      ]

      ;; figure out the exact place
      let temp_x canx + ix
      let temp_y cany + iy

      ;; if the result is off the world its like its empty
      ifelse (temp_x < -75) or (temp_x > 75) or (temp_y < -75) or (temp_y > 75) [ set total total + 0.5 ] [
        ifelse ((count residents with [xcor = temp_x and ycor = temp_y]) > 0) [ ;; calculte the lamda value
          set total (total + sqrt ( ( ((alphaq - ([alphaq] of (one-of residents with [xcor = temp_x and ycor = temp_y]))) ^ 2 ) + ((alphas - ([alphas] of (one-of residents with [xcor = temp_x and ycor = temp_y]))) ^ 2 ) + ((alphac - ([alphac] of (one-of residents with [xcor = temp_x and ycor = temp_y]))) ^ 2 )) / 3 ) ) ]
        [ set total total + 0.5 ]
      ]

      set iy iy + 1
    ]

    set ix ix + 1
  ]

  if total != 0
    [
      ask patch canx cany[
        set comfort (total / n)
        ]
    ]
    ;;]
end

;;----- Randomly select sites, compare utility to resident, and select and move to the best one.
to evaluate
  let xlist 0
  let canx 0
  let ylist 0
  let cany 0
  let util-list 0

    repeat numtests
        [
        ask one-of patches
            [
            set canx [pxcor] of patch-at 0 0
            set cany [pycor] of patch-at 0 0
            ]
                    set-comfort canx cany
        if ( ( count turtles-at canx cany ) = 0 )
            [
            set xlist ( sentence xlist canx )
            set ylist ( sentence ylist cany )
            set util-list (sentence util-list (( ( 1 - abs( 1 - ( 1 / [ sddist ] of ( patch canx cany ) ) )) ^ alphas ) *
                 ( (1 - abs (1 - ( [ quality ] of (patch canx cany ))) ) ^ alphaq )  *   (( 1 - abs(0 - ( [comfort] of ( patch canx cany ) ) )) ^ alphac)))            ]
        ]
     set selx item ( position ( max util-list ) util-list ) xlist
     set sely item ( position ( max util-list ) util-list ) ylist
     setxy selx sely
     set utility item ( position ( max util-list ) util-list ) util-list
end

;;----- Start at last resident and move out in a random direction, selecting the first available site.
to locate-service
    create-services 1
    ask max-one-of services [who]
        [
        set color red
        set shape "box"
        setxy selx sely
        while [ any? other turtles-here ]
             [
             rt ( random 360 )
             fd 1
             ]
        if ( luab? = true )     ;;; reduce aesthetic quality if luab? is switched on
               [
               ask patch-here [ set quality ( quality * 0.75 ) ]
               ask neighbors [ set quality ( quality * 0.9 ) ]
               ask neighbors4 [ set quality ( quality * 0.9 ) ]
               set pcolor scale-color green quality ( 0.1 * ( max-pxcor * 1.414 ) ) ( 0.9 * ( max-pxcor * 1.414 ) )
               ask neighbors
                      [
                      set pcolor scale-color green quality
                      ( 0.1 * ( max-pxcor * 1.414 ) )
                      ( 0.9 * ( max-pxcor * 1.414 ) )
                      ]
              ]
        ask patch-here [set ltype 1]
         ]
    ask patches
        [ set sddist min [distance myself] of services ]
 end

;; Basic statistics
to do-plots
   set-current-plot "Development Beyond Radius"
   plot count residents with
     [ ( distancexy 0 0 ) >= radius ]
end

;; Set some basic statistics variables for analysis
to-report mru
  report mean [utility] of residents
end

to-report vru
  report variance [utility] of residents
end

to-report dor
  report count residents with [(distancexy 0 0) > radius]
end

to-report gini
  let totaly sum [utility] of residents
  let x 1 / count residents
  let y 0
  let yc 0
  let yclast 0
  let ysum 0
  let ab 0
  let tsum 0
  foreach sort-on [utility] residents
  [
    the-resident -> ask the-resident
  [set y utility
   set yc yc + y / totaly
   set ysum yc + yclast
   set ab ysum * x
   set tsum tsum + ab
   set yclast yc
  ]
  ]
  report abs(1 - tsum)
end
@#$#@#$#@
GRAPHICS-WINDOW
12
10
708
706
-1
-1
4.61
1
10
1
1
1
0
0
0
1
-75
75
-75
75
0
0
1
ticks
30

SLIDER
764
197
960
230
smoothness
smoothness
0
30
22
1
1
NIL
HORIZONTAL

BUTTON
764
69
840
102
SETUP
setup
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
764
147
840
180
GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
764
231
960
264
numtests
numtests
0
30
15
1
1
NIL
HORIZONTAL

SWITCH
764
264
960
297
luab?
luab?
1
1
-1000

PLOT
994
65
1222
271
Development Beyond Radius
time
# cells
0
100
0
100
true
false
"" ""
PENS
"pen-0" 1 0 -5298144 true "" "plot count residents with \n     [ ( distancexy 0 0 ) >= radius ]"

SLIDER
764
297
960
330
radius
radius
0
50
45
1
1
NIL
HORIZONTAL

BUTTON
764
108
840
141
NEXT STEP
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
765
13
1395
43
Normal Case (Replicated Conceptual)
24
0
1

MONITOR
993
286
1114
331
Household Count
count residents
17
1
11

MONITOR
993
350
1050
395
MRU
mean [utility] of residents
17
1
11

MONITOR
1049
350
1106
395
VRU
variance [utility] of residents
17
1
11

MONITOR
1106
350
1164
395
DOR
count residents with [(distancexy 0 0) > radius]
17
1
11

MONITOR
1164
350
1221
395
Gini
gini
17
1
11

SLIDER
764
330
960
363
num-of-experiments
num-of-experiments
0
1000
200
1
1
NIL
HORIZONTAL
@#$#@#$#@
add model documentation here
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

ant
true
0
Polygon -7500403 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7500403 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7500403 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7500403 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7500403 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7500403 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7500403 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7500403 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7500403 true true 249 107 211 147 168 147 168 150 213 150

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee
true
0
Polygon -1184463 true false 151 152 137 77 105 67 89 67 66 74 48 85 36 100 24 116 14 134 0 151 15 167 22 182 40 206 58 220 82 226 105 226 134 222
Polygon -16777216 true false 151 150 149 128 149 114 155 98 178 80 197 80 217 81 233 95 242 117 246 141 247 151 245 177 234 195 218 207 206 211 184 211 161 204 151 189 148 171
Polygon -7500403 true true 246 151 241 119 240 96 250 81 261 78 275 87 282 103 277 115 287 121 299 150 286 180 277 189 283 197 281 210 270 222 256 222 243 212 242 192
Polygon -16777216 true false 115 70 129 74 128 223 114 224
Polygon -16777216 true false 89 67 74 71 74 224 89 225 89 67
Polygon -16777216 true false 43 91 31 106 31 195 45 211
Line -1 false 200 144 213 70
Line -1 false 213 70 213 45
Line -1 false 214 45 203 26
Line -1 false 204 26 185 22
Line -1 false 185 22 170 25
Line -1 false 169 26 159 37
Line -1 false 159 37 156 55
Line -1 false 157 55 199 143
Line -1 false 200 141 162 227
Line -1 false 162 227 163 241
Line -1 false 163 241 171 249
Line -1 false 171 249 190 254
Line -1 false 192 253 203 248
Line -1 false 205 249 218 235
Line -1 false 218 235 200 144

bird1
false
0
Polygon -7500403 true true 2 6 2 39 270 298 297 298 299 271 187 160 279 75 276 22 100 67 31 0

bird2
false
0
Polygon -7500403 true true 2 4 33 4 298 270 298 298 272 298 155 184 117 289 61 295 61 105 0 43

boat1
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7500403 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

boat2
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 157 54 175 79 174 96 185 102 178 112 194 124 196 131 190 139 192 146 211 151 216 154 157 154
Polygon -7500403 true true 150 74 146 91 139 99 143 114 141 123 137 126 131 129 132 139 142 136 126 142 119 147 148 147

boat3
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 37 172 45 188 59 202 79 217 109 220 130 218 147 204 156 158 156 161 142 170 123 170 102 169 88 165 62
Polygon -7500403 true true 149 66 142 78 139 96 141 111 146 139 148 147 110 147 113 131 118 106 126 71

box
true
0
Polygon -7500403 true true 45 255 255 255 255 45 45 45

butterfly1
true
0
Polygon -16777216 true false 151 76 138 91 138 284 150 296 162 286 162 91
Polygon -7500403 true true 164 106 184 79 205 61 236 48 259 53 279 86 287 119 289 158 278 177 256 182 164 181
Polygon -7500403 true true 136 110 119 82 110 71 85 61 59 48 36 56 17 88 6 115 2 147 15 178 134 178
Polygon -7500403 true true 46 181 28 227 50 255 77 273 112 283 135 274 135 180
Polygon -7500403 true true 165 185 254 184 272 224 255 251 236 267 191 283 164 276
Line -7500403 true 167 47 159 82
Line -7500403 true 136 47 145 81
Circle -7500403 true true 165 45 8
Circle -7500403 true true 134 45 6
Circle -7500403 true true 133 44 7
Circle -7500403 true true 133 43 8

circle
false
0
Circle -7500403 true true 35 35 230

person
false
0
Circle -7500403 true true 155 20 63
Rectangle -7500403 true true 158 79 217 164
Polygon -7500403 true true 158 81 110 129 131 143 158 109 165 110
Polygon -7500403 true true 216 83 267 123 248 143 215 107
Polygon -7500403 true true 167 163 145 234 183 234 183 163
Polygon -7500403 true true 195 163 195 233 227 233 206 159

sheep
false
15
Rectangle -1 true true 90 75 270 225
Circle -1 true true 15 75 150
Rectangle -16777216 true false 81 225 134 286
Rectangle -16777216 true false 180 225 238 285
Circle -16777216 true false 1 88 92

spacecraft
true
0
Polygon -7500403 true true 150 0 180 135 255 255 225 240 150 180 75 240 45 255 120 135

thin-arrow
true
0
Polygon -7500403 true true 150 0 0 150 120 150 120 293 180 293 180 150 300 150

truck-down
false
0
Polygon -7500403 true true 225 30 225 270 120 270 105 210 60 180 45 30 105 60 105 30
Polygon -8630108 true false 195 75 195 120 240 120 240 75
Polygon -8630108 true false 195 225 195 180 240 180 240 225

truck-left
false
0
Polygon -7500403 true true 120 135 225 135 225 210 75 210 75 165 105 165
Polygon -8630108 true false 90 210 105 225 120 210
Polygon -8630108 true false 180 210 195 225 210 210

truck-right
false
0
Polygon -7500403 true true 180 135 75 135 75 210 225 210 225 165 195 165
Polygon -8630108 true false 210 210 195 225 180 210
Polygon -8630108 true false 120 210 105 225 90 210

turtle
true
0
Polygon -7500403 true true 138 75 162 75 165 105 225 105 225 142 195 135 195 187 225 195 225 225 195 217 195 202 105 202 105 217 75 225 75 195 105 187 105 135 75 142 75 105 135 105

wolf
false
0
Rectangle -7500403 true true 15 105 105 165
Rectangle -7500403 true true 45 90 105 105
Polygon -7500403 true true 60 90 83 44 104 90
Polygon -16777216 true false 67 90 82 59 97 89
Rectangle -1 true false 48 93 59 105
Rectangle -16777216 true false 51 96 55 101
Rectangle -16777216 true false 0 121 15 135
Rectangle -16777216 true false 15 136 60 151
Polygon -1 true false 15 136 23 149 31 136
Polygon -1 true false 30 151 37 136 43 151
Rectangle -7500403 true true 105 120 263 195
Rectangle -7500403 true true 108 195 259 201
Rectangle -7500403 true true 114 201 252 210
Rectangle -7500403 true true 120 210 243 214
Rectangle -7500403 true true 115 114 255 120
Rectangle -7500403 true true 128 108 248 114
Rectangle -7500403 true true 150 105 225 108
Rectangle -7500403 true true 132 214 155 270
Rectangle -7500403 true true 110 260 132 270
Rectangle -7500403 true true 210 214 232 270
Rectangle -7500403 true true 189 260 210 270
Line -7500403 true 263 127 281 155
Line -7500403 true 281 155 281 192

wolf-left
false
3
Polygon -6459832 true true 117 97 91 74 66 74 60 85 36 85 38 92 44 97 62 97 81 117 84 134 92 147 109 152 136 144 174 144 174 103 143 103 134 97
Polygon -6459832 true true 87 80 79 55 76 79
Polygon -6459832 true true 81 75 70 58 73 82
Polygon -6459832 true true 99 131 76 152 76 163 96 182 104 182 109 173 102 167 99 173 87 159 104 140
Polygon -6459832 true true 107 138 107 186 98 190 99 196 112 196 115 190
Polygon -6459832 true true 116 140 114 189 105 137
Rectangle -6459832 true true 109 150 114 192
Rectangle -6459832 true true 111 143 116 191
Polygon -6459832 true true 168 106 184 98 205 98 218 115 218 137 186 164 196 176 195 194 178 195 178 183 188 183 169 164 173 144
Polygon -6459832 true true 207 140 200 163 206 175 207 192 193 189 192 177 198 176 185 150
Polygon -6459832 true true 214 134 203 168 192 148
Polygon -6459832 true true 204 151 203 176 193 148
Polygon -6459832 true true 207 103 221 98 236 101 243 115 243 128 256 142 239 143 233 133 225 115 214 114

wolf-right
false
3
Polygon -6459832 true true 170 127 200 93 231 93 237 103 262 103 261 113 253 119 231 119 215 143 213 160 208 173 189 187 169 190 154 190 126 180 106 171 72 171 73 126 122 126 144 123 159 123
Polygon -6459832 true true 201 99 214 69 215 99
Polygon -6459832 true true 207 98 223 71 220 101
Polygon -6459832 true true 184 172 189 234 203 238 203 246 187 247 180 239 171 180
Polygon -6459832 true true 197 174 204 220 218 224 219 234 201 232 195 225 179 179
Polygon -6459832 true true 78 167 95 187 95 208 79 220 92 234 98 235 100 249 81 246 76 241 61 212 65 195 52 170 45 150 44 128 55 121 69 121 81 135
Polygon -6459832 true true 48 143 58 141
Polygon -6459832 true true 46 136 68 137
Polygon -6459832 true true 45 129 35 142 37 159 53 192 47 210 62 238 80 237
Line -16777216 false 74 237 59 213
Line -16777216 false 59 213 59 212
Line -16777216 false 58 211 67 192
Polygon -6459832 true true 38 138 66 149
Polygon -6459832 true true 46 128 33 120 21 118 11 123 3 138 5 160 13 178 9 192 0 199 20 196 25 179 24 161 25 148 45 140
Polygon -6459832 true true 67 122 96 126 63 144
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0
-0.2 0 0 1
0 1 1 0
0.2 0 0 1
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@

@#$#@#$#@
