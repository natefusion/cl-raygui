(defpackage #:cl-raygui
  (:nicknames #:raygui)
  (:use #:cl #:cffi #:alexandria #:cl-raylib)
  (:export
   #:make-gui-style-prop
   #:gui-style-prop-control-id
   #:gui-style-prop-property-id
   #:gui-style-prop-property-value

   #:+gui-state-normal+
   #:+gui-state-focused+ 
   #:+gui-state-pressed+ 
   #:+gui-state-disabled+

   #:+gui-text-align-left+ 
   #:+gui-text-align-center+
   #:+gui-text-align-right+

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
   #:+combo-button-padding+

   #:+arrow-padding+ 
   #:+dropdown-items-padding+

   #:+text-inner-padding+
   #:+text-lines-padding+
   #:+color-seleceted-fg+
   #:+color-selected-bg+

   #:+spin-button-width+
   #:+spin-button-padding+

   #:+arrows-size+
   #:+arrows-visible+
   #:+scroll-slider-padding+
   #:+scroll-slider-size+
   #:+scroll-padding+
   #:+scroll-speed+

   #:+scrollbar-left-side+ 
   #:+scrollbar-right-side+

   #:+list-items-height+
   #:+list-items-padding+
   #:+scrollbar-width+
   #:+scrollbar-side+

   #:+color-selector-size+
   #:+huebar-width+
   #:+huebar-padding+
   #:+huebar-selector-height+
   #:+huebar-selector-overflow+


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
   #:gui-scroll-bar
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
   #:gui-set-icon-pixel
   #:gui-clear-icon-pixel
   #:gui-check-icon-pixel))

