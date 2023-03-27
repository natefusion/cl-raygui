(defpackage #:cl-raygui
  (:nicknames #:raygui)
  (:use #:cl #:cffi #:alexandria #:cl-raylib)
  (:export
   #:reset-state
   
   #:make-gui-style-prop
   #:gui-style-prop-control-id
   #:gui-style-prop-property-id
   #:gui-style-prop-property-value

   #:+state-normal+
   #:+state-focused+ 
   #:+state-pressed+ 
   #:+state-disabled+

   #:+text-align-left+ 
   #:+text-align-center+
   #:+text-align-right+

   #:+default+      
   #:+label+        
   #:+button+
   #:+toggle+       
   #:+slider+       
   #:+progressbar+  
   #:+checkbox+     
   #:+combobox+     
   #:+dropdownbox+  
   #:+textbox+      
   #:+valuebox+    
   #:+spinner+     
   #:+listview+    
   #:+colorpicker+ 
   #:+scrollbar+   
   #:+statusbar+

   #:+border-color-normal+   
   #:+base-color-normal+    
   #:+text-color-normal+    
   #:+border-color-focused+ 
   #:+base-color-focused+   
   #:+text-color-focused+   
   #:+border-color-pressed+ 
   #:+base-color-pressed+   
   #:+text-color-pressed+   
   #:+border-color-disabled+
   #:+base-color-disabled+ 
   #:+text-color-disabled+ 
   #:+border-width+        
   #:+text-padding+        
   #:+text-alignment+      
   #:+reserved+

   #:+text-size+
   #:+text-spacing+
   #:+line-color+
   #:+background-color+

   #:+group-padding+

   #:+slider-width+
   #:+slider-padding+

   #:+progress-padding+

   #:+check-padding+

   #:+combo-button-width+
   #:+combo-button-spacing+

   #:+arrow-padding+ 
   #:+dropdown-items-spacing+

   #:+text-inner-padding+
   #:+text-lines-spacing+
   #:+color-seleceted-fg+
   #:+color-selected-bg+

   #:+spin-button-width+
   #:+spin-button-spacing+

   #:+arrows-size+
   #:+arrows-visible+
   #:+scroll-slider-padding+
   #:+scroll-slider-size+
   #:+scroll-padding+
   #:+scroll-speed+

   #:+scrollbar-left-side+ 
   #:+scrollbar-right-side+

   #:+list-items-height+
   #:+list-items-spacing+
   #:+scrollbar-width+
   #:+scrollbar-side+

   #:+color-selector-size+
   #:+huebar-width+
   #:+huebar-padding+
   #:+huebar-selector-height+
   #:+huebar-selector-overflow+

   #:+scrollbar-left-side+
   #:+scrollbar-right-side+


   #:gui-enable
   #:gui-disable
   #:gui-lock
   #:gui-unlock
   #:gui-is-locked
   #:gui-fade
   #:gui-set-state
   #:gui-get-state
   #:gui-set-font
   #:gui-get-font
   #:gui-set-style
   #:gui-get-style
   #:gui-window-box
   #:gui-group-box
   #:gui-line
   #:gui-panel
   #:gui-scroll-panel
   #:gui-label
   #:gui-button
   #:gui-label-button
   #:gui-toggle
   #:gui-toggle-group
   #:gui-check-box
   #:gui-combo-box
   #:gui-dropdown-box
   #:gui-spinner
   #:gui-value-box
   #:gui-text-box
   #:gui-text-box-multi
   #:gui-slider
   #:gui-slider-bar
   #:gui-progress-bar
   #:gui-status-bar
   #:gui-dummy-rect
   #:gui-grid
   #:gui-list-view
   #:gui-list-view-ex
   #:gui-message-box
   #:gui-text-input-box
   #:gui-color-picker
   #:gui-color-panel
   #:gui-color-bar-alpha
   #:gui-color-bar-hue
   #:gui-load-style
   #:gui-load-style-default
   #:gui-icon-text
   #:gui-draw-icon
   #:gui-get-icons
   #:gui-get-icon-data
   #:gui-set-icon-data
   #:gui-set-icon-scale
   #:gui-set-icon-pixel
   #:gui-clear-icon-pixel
   #:gui-check-icon-pixel

   #:+ICON-NONE+
   #:+ICON-FOLDER-FILE-OPEN+
   #:+ICON-FILE-SAVE-CLASSIC+
   #:+ICON-FOLDER-OPEN+
   #:+ICON-FOLDER-SAVE+
   #:+ICON-FILE-OPEN+
   #:+ICON-FILE-SAVE+
   #:+ICON-FILE-EXPORT+
   #:+ICON-FILE-ADD+
   #:+ICON-FILE-DELETE+
   #:+ICON-FILETYPE-TEXT+
   #:+ICON-FILETYPE-AUDIO+
   #:+ICON-FILETYPE-IMAGE+
   #:+ICON-FILETYPE-PLAY+
   #:+ICON-FILETYPE-VIDEO+
   #:+ICON-FILETYPE-INFO+
   #:+ICON-FILE-COPY+
   #:+ICON-FILE-CUT+
   #:+ICON-FILE-PASTE+
   #:+ICON-CURSOR-HAND+
   #:+ICON-CURSOR-POINTER+
   #:+ICON-CURSOR-CLASSIC+
   #:+ICON-PENCIL+
   #:+ICON-PENCIL-BIG+
   #:+ICON-BRUSH-CLASSIC+
   #:+ICON-BRUSH-PAINTER+
   #:+ICON-WATER-DROP+
   #:+ICON-COLOR-PICKER+
   #:+ICON-RUBBER+
   #:+ICON-COLOR-BUCKET+
   #:+ICON-TEXT-T+
   #:+ICON-TEXT-A+
   #:+ICON-SCALE+
   #:+ICON-RESIZE+
   #:+ICON-FILTER-POINT+
   #:+ICON-FILTER-BILINEAR+
   #:+ICON-CROP+
   #:+ICON-CROP-ALPHA+
   #:+ICON-SQUARE-TOGGLE+
   #:+ICON-SYMMETRY+
   #:+ICON-SYMMETRY-HORIZONTAL+
   #:+ICON-SYMMETRY-VERTICAL+
   #:+ICON-LENS+
   #:+ICON-LENS-BIG+
   #:+ICON-EYE-ON+
   #:+ICON-EYE-OFF+
   #:+ICON-FILTER-TOP+
   #:+ICON-FILTER+
   #:+ICON-TARGET-POINT+
   #:+ICON-TARGET-SMALL+
   #:+ICON-TARGET-BIG+
   #:+ICON-TARGET-MOVE+
   #:+ICON-CURSOR-MOVE+
   #:+ICON-CURSOR-SCALE+
   #:+ICON-CURSOR-SCALE-RIGHT+
   #:+ICON-CURSOR-SCALE-LEFT+
   #:+ICON-UNDO+
   #:+ICON-REDO+
   #:+ICON-REREDO+
   #:+ICON-MUTATE+
   #:+ICON-ROTATE+
   #:+ICON-REPEAT+
   #:+ICON-SHUFFLE+
   #:+ICON-EMPTYBOX+
   #:+ICON-TARGET+
   #:+ICON-TARGET-SMALL-FILL+
   #:+ICON-TARGET-BIG-FILL+
   #:+ICON-TARGET-MOVE-FILL+
   #:+ICON-CURSOR-MOVE-FILL+
   #:+ICON-CURSOR-SCALE-FILL+
   #:+ICON-CURSOR-SCALE-RIGHT-FILL+
   #:+ICON-CURSOR-SCALE-LEFT-FILL+
   #:+ICON-UNDO-FILL+
   #:+ICON-REDO-FILL+
   #:+ICON-REREDO-FILL+
   #:+ICON-MUTATE-FILL+
   #:+ICON-ROTATE-FILL+
   #:+ICON-REPEAT-FILL+
   #:+ICON-SHUFFLE-FILL+
   #:+ICON-EMPTYBOX-SMALL+
   #:+ICON-BOX+
   #:+ICON-BOX-TOP+
   #:+ICON-BOX-TOP-RIGHT+
   #:+ICON-BOX-RIGHT+
   #:+ICON-BOX-BOTTOM-RIGHT+
   #:+ICON-BOX-BOTTOM+
   #:+ICON-BOX-BOTTOM-LEFT+
   #:+ICON-BOX-LEFT+
   #:+ICON-BOX-TOP-LEFT+
   #:+ICON-BOX-CENTER+
   #:+ICON-BOX-CIRCLE-MASK+
   #:+ICON-POT+
   #:+ICON-ALPHA-MULTIPLY+
   #:+ICON-ALPHA-CLEAR+
   #:+ICON-DITHERING+
   #:+ICON-MIPMAPS+
   #:+ICON-BOX-GRID+
   #:+ICON-GRID+
   #:+ICON-BOX-CORNERS-SMALL+
   #:+ICON-BOX-CORNERS-BIG+
   #:+ICON-FOUR-BOXES+
   #:+ICON-GRID-FILL+
   #:+ICON-BOX-MULTISIZE+
   #:+ICON-ZOOM-SMALL+
   #:+ICON-ZOOM-MEDIUM+
   #:+ICON-ZOOM-BIG+
   #:+ICON-ZOOM-ALL+
   #:+ICON-ZOOM-CENTER+
   #:+ICON-BOX-DOTS-SMALL+
   #:+ICON-BOX-DOTS-BIG+
   #:+ICON-BOX-CONCENTRIC+
   #:+ICON-BOX-GRID-BIG+
   #:+ICON-OK-TICK+
   #:+ICON-CROSS+
   #:+ICON-ARROW-LEFT+
   #:+ICON-ARROW-RIGHT+
   #:+ICON-ARROW-DOWN+
   #:+ICON-ARROW-UP+
   #:+ICON-ARROW-LEFT-FILL+
   #:+ICON-ARROW-RIGHT-FILL+
   #:+ICON-ARROW-DOWN-FILL+
   #:+ICON-ARROW-UP-FILL+
   #:+ICON-AUDIO+
   #:+ICON-FX+
   #:+ICON-WAVE+
   #:+ICON-WAVE-SINUS+
   #:+ICON-WAVE-SQUARE+
   #:+ICON-WAVE-TRIANGULAR+
   #:+ICON-CROSS-SMALL+
   #:+ICON-PLAYER-PREVIOUS+
   #:+ICON-PLAYER-PLAY-BACK+
   #:+ICON-PLAYER-PLAY+
   #:+ICON-PLAYER-PAUSE+
   #:+ICON-PLAYER-STOP+
   #:+ICON-PLAYER-NEXT+
   #:+ICON-PLAYER-RECORD+
   #:+ICON-MAGNET+
   #:+ICON-LOCK-CLOSE+
   #:+ICON-LOCK-OPEN+
   #:+ICON-CLOCK+
   #:+ICON-TOOLS+
   #:+ICON-GEAR+
   #:+ICON-GEAR-BIG+
   #:+ICON-BIN+
   #:+ICON-HAND-POINTER+
   #:+ICON-LASER+
   #:+ICON-COIN+
   #:+ICON-EXPLOSION+
   #:+ICON-1UP+
   #:+ICON-PLAYER+
   #:+ICON-PLAYER-JUMP+
   #:+ICON-KEY+
   #:+ICON-DEMON+
   #:+ICON-TEXT-POPUP+
   #:+ICON-GEAR-EX+
   #:+ICON-CRACK+
   #:+ICON-CRACK-POINTS+
   #:+ICON-STAR+
   #:+ICON-DOOR+
   #:+ICON-EXIT+
   #:+ICON-MODE-2D+
   #:+ICON-MODE-3D+
   #:+ICON-CUBE+
   #:+ICON-CUBE-FACE-TOP+
   #:+ICON-CUBE-FACE-LEFT+
   #:+ICON-CUBE-FACE-FRONT+
   #:+ICON-CUBE-FACE-BOTTOM+
   #:+ICON-CUBE-FACE-RIGHT+
   #:+ICON-CUBE-FACE-BACK+
   #:+ICON-CAMERA+
   #:+ICON-SPECIAL+
   #:+ICON-LINK-NET+
   #:+ICON-LINK-BOXES+
   #:+ICON-LINK-MULTI+
   #:+ICON-LINK+
   #:+ICON-LINK-BROKE+
   #:+ICON-TEXT-NOTES+
   #:+ICON-NOTEBOOK+
   #:+ICON-SUITCASE+
   #:+ICON-SUITCASE-ZIP+
   #:+ICON-MAILBOX+
   #:+ICON-MONITOR+
   #:+ICON-PRINTER+
   #:+ICON-PHOTO-CAMERA+
   #:+ICON-PHOTO-CAMERA-FLASH+
   #:+ICON-HOUSE+
   #:+ICON-HEART+
   #:+ICON-CORNER+
   #:+ICON-VERTICAL-BARS+
   #:+ICON-VERTICAL-BARS-FILL+
   #:+ICON-LIFE-BARS+
   #:+ICON-INFO+
   #:+ICON-CROSSLINE+
   #:+ICON-HELP+
   #:+ICON-FILETYPE-ALPHA+
   #:+ICON-FILETYPE-HOME+
   #:+ICON-LAYERS-VISIBLE+
   #:+ICON-LAYERS+
   #:+ICON-WINDOW+
   #:+ICON-HIDPI+
   #:+ICON-FILETYPE-BINARY+
   #:+ICON-HEX+
   #:+ICON-SHIELD+
   #:+ICON-FILE-NEW+
   #:+ICON-FOLDER-ADD+
   #:+ICON-ALARM+
   #:+ICON-206+
   #:+ICON-207+
   #:+ICON-208+
   #:+ICON-209+
   #:+ICON-210+
   #:+ICON-211+
   #:+ICON-212+
   #:+ICON-213+
   #:+ICON-214+
   #:+ICON-215+
   #:+ICON-216+
   #:+ICON-217+
   #:+ICON-218+
   #:+ICON-219+
   #:+ICON-220+
   #:+ICON-221+
   #:+ICON-222+
   #:+ICON-223+
   #:+ICON-224+
   #:+ICON-225+
   #:+ICON-226+
   #:+ICON-227+
   #:+ICON-228+
   #:+ICON-229+
   #:+ICON-230+
   #:+ICON-231+
   #:+ICON-232+
   #:+ICON-233+
   #:+ICON-234+
   #:+ICON-235+
   #:+ICON-236+
   #:+ICON-237+
   #:+ICON-238+
   #:+ICON-239+
   #:+ICON-240+
   #:+ICON-241+
   #:+ICON-242+
   #:+ICON-243+
   #:+ICON-244+
   #:+ICON-245+
   #:+ICON-246+
   #:+ICON-247+
   #:+ICON-248+
   #:+ICON-249+
   #:+ICON-250+
   #:+ICON-251+
   #:+ICON-252+
   #:+ICON-253+
   #:+ICON-254+
   #:+ICON-255+))

