(in-package #:cl-raygui)

;; // Style property
;; typedef struct GuiStyleProp {
;;     unsigned short controlId;
;;     unsigned short propertyId;
;;     unsigned int propertyValue;
;; } GuiStyleProp;

(defcstruct (%gui-style-prop :class gui-style-prop-type)
  "Style property"
  (control-id :unsigned-short)
  (property-id :unsigned-short)
  (property-value :unsigned-int))

(defstruct gui-style-prop
  control-id property-id property-value)

(defmethod translate-into-foreign-memory (object (type gui-style-prop-type) pointer)
  (with-foreign-slots ((control-id property-id property-value) pointer (:struct %gui-style-prop))
    (setf control-id (coerce (gui-style-prop-control-id object) 'unsigned-short))
    (setf property-id (coerce (gui-style-prop-property-id object) 'unsigned-short))
    (setf property-value (coerce (gui-style-prop-property-value object) 'unsigned-int))))

(defmethod translate-from-foreign (pointer (type gui-style-prop-type))
  (with-foreign-slots ((control-id property-id property-value) pointer (:struct %gui-style-prop))
    (make-gui-style-prop :control-id control-id :property-id property-id :property-value property-value)))

;; // Gui control state
;; typedef enum {
;;     STATE_NORMAL = 0,
;;     STATE_FOCUSED,
;;     STATE_PRESSED,
;;     STATE_DISABLED,
;; } GuiState;

(defcenum gui-state
  (:state-normal 0)
  (:state-focused 1)
  (:state-pressed 2)
  (:state-disabled 3))

;; // Gui control text alignment
;; typedef enum {
;;     TEXT_ALIGN_LEFT = 0,
;;     TEXT_ALIGN_CENTER,
;;     TEXT_ALIGN_RIGHT,
;; } GuiTextAlignment;

(defcenum gui-text-alignment
  (:text-align-left 0)
  (:text-align-center 1)
  (:text-align-right 2))

;; // Gui controls
;; typedef enum {
;;     // Default -> populates to all controls when set
;;     DEFAULT = 0,
;;     // Basic controls
;;     LABEL,          // Used also for: LABELBUTTON
;;     BUTTON,
;;     TOGGLE,         // Used also for: TOGGLEGROUP
;;     SLIDER,         // Used also for: SLIDERBAR
;;     PROGRESSBAR,
;;     CHECKBOX,
;;     COMBOBOX,
;;     DROPDOWNBOX,
;;     TEXTBOX,        // Used also for: TEXTBOXMULTI
;;     VALUEBOX,
;;     SPINNER,        // Uses: BUTTON, VALUEBOX
;;     LISTVIEW,
;;     COLORPICKER,
;;     SCROLLBAR,
;;     STATUSBAR
;; } GuiControl;

(defcenum gui-control
  (:default 0)
  (:label 1)
  (:button 2)
  (:toggle 3)
  (:slider 4)
  (:progressbar 5)
  (:checkbox 6)
  (:combobox 7)
  (:dropdownbox 8)
  (:textbox 9)
  (:valuebox 10)
  (:spinner 11)
  (:listview 12)
  (:colorpicker 13)
  (:scrollbar 14)
  (:statusbar 15))

;; // Gui base properties for every control
;; // NOTE: RAYGUI_MAX_PROPS_BASE properties (by default 16 properties)
;; typedef enum {
;;     BORDER_COLOR_NORMAL = 0,
;;     BASE_COLOR_NORMAL,
;;     TEXT_COLOR_NORMAL,
;;     BORDER_COLOR_FOCUSED,
;;     BASE_COLOR_FOCUSED,
;;     TEXT_COLOR_FOCUSED,
;;     BORDER_COLOR_PRESSED,
;;     BASE_COLOR_PRESSED,
;;     TEXT_COLOR_PRESSED,
;;     BORDER_COLOR_DISABLED,
;;     BASE_COLOR_DISABLED,
;;     TEXT_COLOR_DISABLED,
;;     BORDER_WIDTH,
;;     TEXT_PADDING,
;;     TEXT_ALIGNMENT,
;;     RESERVED
;; } GuiControlProperty;

(defcenum gui-control-property
  (:border-color-normal 0)
  (:base-color-normal 1)
  (:text-color-normal 2)
  (:border-color-focused 3)
  (:base-color-focused 4)
  (:text-color-focused 5)
  (:border-color-pressed 6)
  (:base-color-pressed 7)
  (:text-color-pressed 8)
  (:border-color-disabled 9)
  (:base-color-disabled 10)
  (:text-color-disabled 11)
  (:border-width 12)
  (:text-padding 13)
  (:text-alignment 14)
  (:reserved 15))

;; // Gui extended properties depend on control
;; // NOTE: RAYGUI_MAX_PROPS_EXTENDED properties (by default 8 properties)
;; //----------------------------------------------------------------------------------

;; // DEFAULT extended properties
;; // NOTE: Those properties are common to all controls or global
;; typedef enum {
;;     TEXT_SIZE = 16,             // Text size (glyphs max height)
;;     TEXT_SPACING,               // Text spacing between glyphs
;;     LINE_COLOR,                 // Line control color
;;     BACKGROUND_COLOR,           // Background color
;; } GuiDefaultProperty;

(defcenum gui-default-property
  (:text-size 16)
  (:text-spacing 17)
  (:line-color 18)
  (:background-color 19))

;; // Label
;; //typedef enum { } GuiLabelProperty;

;; // Button/Spinner
;; //typedef enum { } GuiButtonProperty;

;; // Toggle/ToggleGroup
;; typedef enum {
;;     GROUP_PADDING = 16,         // ToggleGroup separation between toggles
;; } GuiToggleProperty;

(defcenum gui-toggle-property
  (:group-padding 16))

;; // Slider/SliderBar
;; typedef enum {
;;     SLIDER_WIDTH = 16,          // Slider size of internal bar
;;     SLIDER_PADDING              // Slider/SliderBar internal bar padding
;; } GuiSliderProperty;

(defcenum gui-slider-property
  (:slider-width 16)
  (:slider-padding 17))

;; // ProgressBar
;; typedef enum {
;;     PROGRESS_PADDING = 16,      // ProgressBar internal padding
;; } GuiProgressBarProperty;

(defcenum gui-progress-bar-property
  (:progress-padding 16))

;; // ScrollBar
;; typedef enum {
;;     ARROWS_SIZE = 16,
;;     ARROWS_VISIBLE,
;;     SCROLL_SLIDER_PADDING,      // (SLIDERBAR, SLIDER_PADDING)
;;     SCROLL_SLIDER_SIZE,
;;     SCROLL_PADDING,
;;     SCROLL_SPEED,
;; } GuiScrollBarProperty;

(defcenum gui-scroll-bar-property
  (:arrows-size 16)
  (:arrows-visible 17)
  (:scroll-slider-padding 18)
  (:scroll-slider-size 19)
  (:scroll-padding 20)
  (:scroll-speed 21))

;; // CheckBox
;; typedef enum {
;;     CHECK_PADDING = 16          // CheckBox internal check padding
;; } GuiCheckBoxProperty;

(defcenum gui-check-box-property
  (:check-padding 16))

;; // ComboBox
;; typedef enum {
;;     COMBO_BUTTON_WIDTH = 16,    // ComboBox right button width
;;     COMBO_BUTTON_SPACING        // ComboBox button separation
;; } GuiComboBoxProperty;

(defcenum gui-combo-box-property
  (:combo-button-width 16)
  (:combo-button-spacing 17))

;; // DropdownBox
;; typedef enum {
;;     ARROW_PADDING = 16,         // DropdownBox arrow separation from border and items
;;     DROPDOWN_ITEMS_SPACING      // DropdownBox items separation
;; } GuiDropdownBoxProperty;

(defcenum gui-dropdown-box-property
  (:arrow-padding 16)
  (:dropdown-items-spacing 17))

;; // TextBox/TextBoxMulti/ValueBox/Spinner
;; typedef enum {
;;     TEXT_INNER_PADDING = 16,    // TextBox/TextBoxMulti/ValueBox/Spinner inner text padding
;;     TEXT_LINES_SPACING,         // TextBoxMulti lines separation
;;     TEXT_ALIGNMENT_VERTICAL,    // TextBoxMulti vertical alignment: 0-CENTERED, 1-UP, 2-DOWN
;;     TEXT_MULTILINE,             // TextBox supports multiple lines
;;     TEXT_WRAP_MODE              // TextBox wrap mode for multiline: 0-NO_WRAP, 1-CHAR_WRAP, 2-WORD_WRAP
;; } GuiTextBoxProperty;

(defcenum gui-text-box-property
  (:text-inner-padding 16)
  (:text-lines-spacing 17)
  (:text-alignment-vertical 18)
  (:text-multiline 19)
  (:text-wrap-mode 20))

;; // Spinner
;; typedef enum {
;;     SPIN_BUTTON_WIDTH = 16,     // Spinner left/right buttons width
;;     SPIN_BUTTON_SPACING,        // Spinner buttons separation
;; } GuiSpinnerProperty;

(defcenum gui-spinner-property
  (:spin-button-width 16)
  (:spin-button-spacing 17))

;; // ListView
;; typedef enum {
;;     LIST_ITEMS_HEIGHT = 16,     // ListView items height
;;     LIST_ITEMS_SPACING,         // ListView items separation
;;     SCROLLBAR_WIDTH,            // ListView scrollbar size (usually width)
;;     SCROLLBAR_SIDE,             // ListView scrollbar side (0-left, 1-right)
;; } GuiListViewProperty;

(defcenum gui-list-view-property
  (:list-items-height 16)
  (:list-items-spacing 17)
  (:scrollbar-width 18)
  (:scrollbar-side 19))

;; // ColorPicker
;; typedef enum {
;;     COLOR_SELECTOR_SIZE = 16,
;;     HUEBAR_WIDTH,               // ColorPicker right hue bar width
;;     HUEBAR_PADDING,             // ColorPicker right hue bar separation from panel
;;     HUEBAR_SELECTOR_HEIGHT,     // ColorPicker right hue bar selector height
;;     HUEBAR_SELECTOR_OVERFLOW    // ColorPicker right hue bar selector overflow
;; } GuiColorPickerProperty;

(defcenum gui-color-picker-property
  (:color-selector-size 16)
  (:huebar-width 17)
  (:huebar-padding 18)
  (:huebar-selector-height 19)
  (:huebar-selector-overflow 20))

;; #define SCROLLBAR_LEFT_SIDE     0
;; #define SCROLLBAR_RIGHT_SIDE    1

(define-constant +scrollbar-left-side+ 0)
(define-constant +scrollbar-right-side+ 1)

;; //----------------------------------------------------------------------------------
;; // Global Variables Definition
;; //----------------------------------------------------------------------------------
;; // ...

;; //----------------------------------------------------------------------------------
;; // Module Functions Declaration
;; //----------------------------------------------------------------------------------

;; #if defined(__cplusplus)
;; extern "C" {            // Prevents name mangling of functions
;; #endif

;; // Global gui state control functions
;; // Global gui state control functions
;; RAYGUIAPI void GuiEnable(void);                                         // Enable gui controls (global state)
(defcfun "GuiEnable" :void
  "Enable gui controls (global state)")

;; RAYGUIAPI void GuiDisable(void);                                        // Disable gui controls (global state)
(defcfun "GuiDisable" :void
  "Disable gui controls (global state)")

;; RAYGUIAPI void GuiLock(void);                                           // Lock gui controls (global state)
(defcfun "GuiLock" :void
  "Lock gui controls (global state)")

;; RAYGUIAPI void GuiUnlock(void);                                         // Unlock gui controls (global state)
(defcfun "GuiUnlock" :void
  "Unlock gui controls (global state)")

;; RAYGUIAPI bool GuiIsLocked(void);                                       // Check if gui is locked (global state)
(defcfun ("GuiIsLocked" %gui-is-locked) :int)

;; bool is broken in cffi. this is a workaround
(defmacro gui-is-locked ()
  "Check if gui is locked (global state)"
  (int-bool (%gui-is-locked)))

;; RAYGUIAPI void GuiFade(float alpha);                                    // Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f
(defcfun "GuiFade" :void
  "Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f"
  (alpha :float))

;; RAYGUIAPI void GuiSetState(int state);                                  // Set gui state (global state)
(defcfun "GuiSetState" :void
  "Set gui state (global state)"
  (state :int))

;; RAYGUIAPI int GuiGetState(void);                                        // Get gui state (global state)
(defcfun "GuiGetState" :int
  "Get gui state (global state)")

;; // Font set/get functions
;; RAYGUIAPI void GuiSetFont(Font font);                                   // Set gui custom font (global state)
(defcfun "GuiSetFont" :void
  "Set gui custom font (global state)"
  (font (:struct cl-raylib::%font)))

;; RAYGUIAPI Font GuiGetFont(void);                                        // Get gui custom font (global state)
(defcfun "GuiGetFont" (:struct cl-raylib::%font)
  "Get gui custom font (global state)")

;; // Style set/get functions
;; RAYGUIAPI void GuiSetStyle(int control, int property, int value);       // Set one style property
(defcfun "GuiSetStyle" :void
  "Set one style property"
  (control :int)
  (property :int)
  (value :int))

;; RAYGUIAPI int GuiGetStyle(int control, int property);                   // Get one style property
(defcfun "GuiGetStyle" :int
  "Get one style property"
  (control :int)
  (property :int))

;; // Container/separator controls, useful for controls organization
;; RAYGUIAPI bool GuiWindowBox(Rectangle bounds, const char *title);                                       // Window Box control, shows a window that can be closed
(defcfun ("GuiWindowBox" %gui-window-box) :int
  (bounds (:struct cl-raylib::%rectangle))
  (title :string))

(defmacro gui-window-box (bounds title)
  "Window Box control, shows a window that can be closed"
  `(int-bool (%gui-window-box ,bounds ,title)))

;; RAYGUIAPI void GuiGroupBox(Rectangle bounds, const char *text);                                         // Group Box control with text name
(defcfun "GuiGroupBox" :void
  "Group box control with text name"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI void GuiLine(Rectangle bounds, const char *text);                                             // Line separator control, could contain text
(defcfun "GuiLine" :void
  "Line separator control, could contain text"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI void GuiPanel(Rectangle bounds, const char *text);                                            // Panel control, useful to group controls
(defcfun "GuiPanel" :void
  "Panel control, useful to group controls"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI int GuiTabBar(Rectangle bounds, const char **text, int count, int *active);                   // Tab Bar control, returns TAB to be closed or -1
(defcfun ("GuiTabBar" %gui-tab-bar) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text (:pointer :string))
  (count :int)
  (active (:pointer :int)))

(defmacro gui-tab-bar (bounds text count active)
  "Tab Bar control, returns TAB to be closed or -1"
  (let ((foreign-text (gensym))
        (foreign-active (gensym)))
    `(cffi:with-foreign-objects ((,foreign-text :string)
                                 (,foreign-active :int))
       (setf (cffi:mem-ref ,foreign-text :string) ,text
             (cffi:mem-ref ,foreign-active :int) ,active)
       (prog1 (%gui-tab-bar ,bounds ,foreign-text ,count ,foreign-active)
         (setf ,text (cffi:mem-ref ,foreign-text :string)
               ,active (cffi:mem-ref ,active :int))))))

;; RAYGUIAPI Rectangle GuiScrollPanel(Rectangle bounds, const char *text, Rectangle content, Vector2 *scroll); // Scroll Panel control
(defcfun ("GuiScrollPanel" %gui-scroll-panel) (:struct cl-raylib::%rectangle)
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (content (:struct cl-raylib::%rectangle))
  (scroll (:pointer (:struct cl-raylib::%vector2))))

(defmacro gui-scroll-panel (bounds text content scroll)
  "Scroll panel control"
  (let ((foreign-scroll (gensym)))
    `(cffi:with-foreign-object (,foreign-scroll '(:struct cl-raylib::%vector2))
       (setf (cffi:mem-ref ,foreign-scroll '(:struct cl-raylib::%vector2)) ,scroll)
       (prog1 (%gui-scroll-panel bounds text content foreign-scroll)
         (setf ,scroll (cffi:mem-ref ,foreign-scroll '(:struct cl-raylib::%vector2)))))))

;; // Basic controls set
;; RAYGUIAPI void GuiLabel(Rectangle bounds, const char *text);                                            // Label control, shows text
(defcfun "GuiLabel" :void
  "Label control, shows text"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI bool GuiButton(Rectangle bounds, const char *text);                                           // Button control, returns true when clicked
(defcfun ("GuiButton" %gui-button) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

(defmacro gui-button (bounds text)
  "Button control, returns true when clicked"
  `(int-bool (%gui-button ,bounds ,text)))

;; RAYGUIAPI bool GuiLabelButton(Rectangle bounds, const char *text);                                      // Label button control, show true when clicked
(defcfun ("GuiLabelButton" %gui-label-button) :int
  "Label button control, show true when clicked"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

(defmacro gui-label-button (bounds text)
  "Label button control, show true when clicked"
  `(int-bool (%gui-label-button ,bounds ,text)))

;; RAYGUIAPI bool GuiToggle(Rectangle bounds, const char *text, bool active);                              // Toggle Button control, returns true when active
(defcfun ("GuiToggle" %gui-toggle) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active :int))

(defmacro gui-toggle (bounds text active)
  "Toggle button control, returns true when active"
  `(int-bool (%gui-toggle ,bounds ,text (bool-int ,active))))

;; RAYGUIAPI int GuiToggleGroup(Rectangle bounds, const char *text, int active);                           // Toggle Group control, returns active toggle index
(defcfun ("GuiToggleGroup" %gui-toggle-group) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active :int))

(defmacro gui-toggle-group (bounds text active)
  "Toggle Group control, returns active toggle index"
  `(%gui-toggle-group ,bounds ,text (bool-int ,active)))

;; RAYGUIAPI bool GuiCheckBox(Rectangle bounds, const char *text, bool checked);                           // Check Box control, returns true when active
(defcfun ("GuiCheckBox" %gui-check-box) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (checked :int))

(defmacro gui-check-box (bounds text checked)
  "Check Box control, returns true when active"
  `(int-bool (%gui-check-box ,bounds ,text (bool-int ,checked))))

;; RAYGUIAPI int GuiComboBox(Rectangle bounds, const char *text, int active);                              // Combo Box control, returns selected item index
(defcfun "GuiComboBox" :int
  "Combo Box control, returns selected item index"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active :int))

;; RAYGUIAPI bool GuiDropdownBox(Rectangle bounds, const char *text, int *active, bool editMode);          // Dropdown Box control, returns selected item
(defcfun ("GuiDropdownBox" %gui-dropdown-box) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active (:pointer :int))
  (edit-mode :int))

(defmacro gui-dropdown-box (bounds text active edit-mode)
  "Dropdown Box control, returns selected item"
  (let ((foreign-active (gensym)))
    `(cffi:with-foreign-object (,foreign-active :int)
       (setf (cffi:mem-ref ,foreign-active :int) ,active)
       (prog1 (int-bool (%gui-dropdown-box ,bounds ,text ,foreign-active (bool-int ,edit-mode)))
         (setf ,active (cffi:mem-ref ,foreign-active :int))))))

;; RAYGUIAPI bool GuiSpinner(Rectangle bounds, const char *text, int *value, int minValue, int maxValue, bool editMode);     // Spinner control, returns selected value
(defcfun ("GuiSpinner" %gui-spinner) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (value (:pointer :int))
  (min-value :int)
  (max-value :int)
  (edit-mode :int))

(defmacro gui-spinner (bounds text value min-value max-value edit-mode)
  "Spinner control, returns selected value"
  (let ((foreign-value (gensym)))
    `(cffi:with-foreign-object (,foreign-value :int)
       (setf (cffi:mem-ref ,foreign-value :int) ,value)
       (prog1 (int-bool (%gui-spinner ,bounds ,text ,foreign-value ,min-value ,max-value (bool-int ,edit-mode)))
         (setf ,value (cffi:mem-ref ,foreign-value :int))))))

;; RAYGUIAPI bool GuiValueBox(Rectangle bounds, const char *text, int *value, int minValue, int maxValue, bool editMode);    // Value Box control, updates input text with numbers
(defcfun ("GuiValueBox" %gui-value-box) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (value (:pointer :int))
  (min-value :int)
  (max-value :int)
  (edit-mode :int))

(defmacro gui-value-box (bounds text value min-value max-value edit-mode)
  "Value Box control, updates input text with numbers"
  (let ((foreign-value (gensym)))
    `(cffi:with-foreign-object (,foreign-value :int)
       (setf (cffi:mem-ref ,foreign-value :int) ,value)
       (prog1 (int-bool (%gui-value-box ,bounds ,text ,foreign-value ,min-value ,max-value (bool-int ,edit-mode)))
         (setf ,value (cffi:mem-ref ,foreign-value :int))))))

;; RAYGUIAPI bool GuiTextBox(Rectangle bounds, char *text, int textSize, bool editMode);                   // Text Box control, updates input text
(defcfun ("GuiTextBox" %gui-text-box) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (text-size :int)
  (edit-mode :int))

(defmacro gui-text-box (bounds text text-size edit-mode)
  "Text Box control, updates input text"
  (let ((foreign-text (gensym))
        (foreign-size (gensym)))
    `(cffi:with-foreign-pointer (,foreign-text ,text-size ,foreign-size)
       (cffi:lisp-string-to-foreign ,text ,foreign-text ,foreign-size)
       (prog1 (int-bool (%gui-text-box ,bounds ,foreign-text ,text-size (bool-int ,edit-mode)))
         (setf ,text (cffi:foreign-string-to-lisp ,foreign-text))))))


;; RAYGUIAPI float GuiSlider(Rectangle bounds, const char *textLeft, const char *textRight, float value, float minValue, float maxValue);       // Slider control, returns selected value
(defcfun "GuiSlider" :float
  "Slider control, returns selected value"
  (bounds (:struct cl-raylib::%rectangle))
  (text-left :string)
  (text-right :string)
  (value :float)
  (min-value :float)
  (max-value :float))

;; RAYGUIAPI float GuiSliderBar(Rectangle bounds, const char *textLeft, const char *textRight, float value, float minValue, float maxValue);    // Slider Bar control, returns selected value
(defcfun "GuiSliderBar" :float
  "Slider bar control, returns selected value"
  (bounds (:struct cl-raylib::%rectangle))
  (text-left :string)
  (text-right :string)
  (value :float)
  (min-value :float)
  (max-value :float))

;; RAYGUIAPI float GuiProgressBar(Rectangle bounds, const char *textLeft, const char *textRight, float value, float minValue, float maxValue);  // Progress Bar control, shows current progress value
(defcfun "GuiProgressBar" :float
  "Progress Bar control, shows current progress value"
  (bounds (:struct cl-raylib::%rectangle))
  (text-left :string)
  (text-right :string)
  (value :float)
  (min-value :float)
  (max-value :float))

;; RAYGUIAPI void GuiStatusBar(Rectangle bounds, const char *text);                                        // Status Bar control, shows info text
(defcfun "GuiStatusBar" :void
  "Status Bar control, shows info text"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI void GuiDummyRec(Rectangle bounds, const char *text);                                         // Dummy control for placeholder
(defcfun "GuiDummyRect" :void
  "Dummy control for placeholer"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI Vector2 GuiGrid(Rectangle bounds, const char *text, float spacing, int subdivs);              // Grid control, returns mouse cell position
(defcfun "GuiGrid" (:struct cl-raylib::%vector2)
  "Grid control, returns mouse cell position"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (spacing :float)
  (subdivs :int))

;; // Advance controls set
;; RAYGUIAPI int GuiListView(Rectangle bounds, const char *text, int *scrollIndex, int active);            // List View control, returns selected list item index
(defcfun "GuiListView" :int
  "List View control, returns selected list item index"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (scroll-index (:pointer :int))
  (active :int))

;; RAYGUIAPI int GuiListViewEx(Rectangle bounds, const char **text, int count, int *focus, int *scrollIndex, int active);      // List View with extended parameters
(defcfun ("GuiListViewEx" %gui-list-view-ex) :int
  "List View with extended parameters"
  (bounds (:struct cl-raylib::%rectangle))
  (text (:pointer :string))
  (count :int)
  (focus (:pointer :int))
  (scroll-index (:pointer :int))
  (active :int))

(defmacro gui-list-view-ex (bounds text count focus scroll-index active)
  "Value Box control, updates input text with numbers"
  (let ((foreign-text (gensym))
        (foreign-focus (gensym))
        (foreign-scroll-index (gensym)))
    `(cffi:with-foreign-objects ((,foreign-text :string)
                                 (,foreign-focus :int)
                                 (,foreign-scroll-index :int))
       (setf (cffi:mem-ref ,foreign-text :string) ,text
             (cffi:mem-ref ,foreign-focus :int) ,focus
             (cffi:mem-ref ,foreign-scroll-index :int) ,scroll-index)
       (prog1 (%gui-list-view-ex ,bounds ,foreign-text ,count ,foreign-focus ,foreign-scroll-index ,active)
         (setf ,text (cffi:mem-ref ,foreign-text :string)
               ,focus (cffi:mem-ref ,foreign-focus :int)
               ,scroll-index (cffi:mem-ref ,foreign-scroll-index :int))))))

;; RAYGUIAPI int GuiMessageBox(Rectangle bounds, const char *title, const char *message, const char *buttons);                 // Message Box control, displays a message
(defcfun "GuiMessageBox" :int
  "Message Box control, displays a message"
  (bounds (:struct cl-raylib::%rectangle))
  (title :string)
  (message :string)
  (buttons :string))

;; RAYGUIAPI int GuiTextInputBox(Rectangle bounds, const char *title, const char *message, const char *buttons, char *text, int textMaxSize, int *secretViewActive);   // Text Input Box control, ask for text, supports secret
(defcfun ("GuiTextInputBox" %gui-text-input-box) :int
  (bounds (:struct cl-raylib::%rectangle))
  (title :string)
  (message :string)
  (buttons :string)
  (text :string)
  (text-max-size :int)
  (secret-view-active (:pointer :int)))

(defmacro gui-list-view-ex (bounds title message buttons text text-max-size secret-view-active)
  "Text Input Box control, ask for text, supports secret"
  (let ((foreign-secret-view-active (gensym)))
    `(cffi:with-foreign-object (,foreign-secret-view-active :int)
       (setf (cffi:mem-ref ,foreign-secret-view-active :int) ,secret-view-active)
       (prog1 (%gui-list-view-ex ,bounds ,title ,message ,buttons ,text ,text-max-size ,foreign-secret-view-active)
         (setf ,secret-view-active (cffi:mem-ref ,foreign-secret-view-active :int) )))))

;; RAYGUIAPI Color GuiColorPicker(Rectangle bounds, const char *text, Color color);                        // Color Picker control (multiple color controls)
(defcfun "GuiColorPicker" :uint
  "Color Picker control, (multiple color controls)"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (color :uint))

;; RAYGUIAPI Color GuiColorPanel(Rectangle bounds, const char *text, Color color);                         // Color Panel control
(defcfun "GuiColorPanel" :uint
  "Color Panel Control"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (color :uint))

;; RAYGUIAPI float GuiColorBarAlpha(Rectangle bounds, const char *text, float alpha);                      // Color Bar Alpha control
(defcfun "GuiColorBarAlpha" :float
  "Color Bar Alpha control"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (alpha :float))

;; RAYGUIAPI float GuiColorBarHue(Rectangle bounds, const char *text, float value);                        // Color Bar Hue control
(defcfun "GuiColorBarHue" :float
  "Color Bar Hue control"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (value :float))

;; // Styles loading functions
;; RAYGUIAPI void GuiLoadStyle(const char *fileName);              // Load style file over global style variable (.rgs)
(defcfun "GuiLoadStyle" :void
  "Load style file over global style variable (.rgs)"
  (file-name :string))

;; RAYGUIAPI void GuiLoadStyleDefault(void);                       // Load style default over global style
(defcfun "GuiLoadStyleDefault" :void
  "Load style default over global style")

;; // Tooltips management functions
;; RAYGUIAPI void GuiEnableTooltip(void);                          // Enable gui tooltips (global state)
(defcfun "GuiEnableTooltip" :void
  "Enable gui tooltips (global state)")

;; RAYGUIAPI void GuiDisableTooltip(void);                         // Disable gui tooltips (global state)
(defcfun "GuiDisableTooltip" :void
  "Disable gui tooltips (global state)")

;; RAYGUIAPI void GuiSetTooltip(const char *tooltip);              // Set tooltip string
(defcfun "GuiSetTooltip" :void
  "Set tooltip string"
  (tooltip :string))

;; // Icons functionality
;; RAYGUIAPI const char *GuiIconText(int iconId, const char *text); // Get text with icon id prepended (if supported)
(defcfun "GuiIconText" :string
  "Get text with icon id prepended (if supported)"
  (icon-id :int)
  (text :string))

;; #if !defined(RAYGUI_NO_ICONS)
;; RAYGUIAPI void GuiSetIconScale(int scale);                      // Set default icon drawing size
(defcfun "GuiSetIconScale" :void
  "Set default icon drawing size"
  (scale :int))

;; RAYGUIAPI unsigned int *GuiGetIcons(void);                      // Get raygui icons data pointer
(defcfun "GuiGetIcons" (:pointer :unsigned-int)
  "Get full icons data pointer")

;; RAYGUIAPI char **GuiLoadIcons(const char *fileName, bool loadIconsName); // Load raygui icons file (.rgi) into internal icons data
(defcfun ("GuiLoadIcons" %gui-load-icons) (:pointer :string)
  (file-name :string)
  (load-icons-name :int))

(defmacro gui-load-icons (file-name load-icons-name)
  (%gui-load-icons file-name (bool-int load-icons-name)))

;; RAYGUIAPI void GuiDrawIcon(int iconId, int posX, int posY, int pixelSize, Color color); // Draw icon using pixel size at specified position
(defcfun "GuiDrawIcon" :void
  "Draw icon using pixel size at specified position"
  (icon-id :int)
  (pos-x :int)
  (pos-y :int)
  (pixel-size :int)
  (color :uint))

;; #if !defined(RAYGUI_CUSTOM_ICONS)
;; //----------------------------------------------------------------------------------
;; // Icons enumeration
;; //----------------------------------------------------------------------------------
;; typedef enum {
;;     ICON_NONE                     = 0,
;;     ICON_FOLDER_FILE_OPEN         = 1,
;;     ICON_FILE_SAVE_CLASSIC        = 2,
;;     ICON_FOLDER_OPEN              = 3,
;;     ICON_FOLDER_SAVE              = 4,
;;     ICON_FILE_OPEN                = 5,
;;     ICON_FILE_SAVE                = 6,
;;     ICON_FILE_EXPORT              = 7,
;;     ICON_FILE_ADD                 = 8,
;;     ICON_FILE_DELETE              = 9,
;;     ICON_FILETYPE_TEXT            = 10,
;;     ICON_FILETYPE_AUDIO           = 11,
;;     ICON_FILETYPE_IMAGE           = 12,
;;     ICON_FILETYPE_PLAY            = 13,
;;     ICON_FILETYPE_VIDEO           = 14,
;;     ICON_FILETYPE_INFO            = 15,
;;     ICON_FILE_COPY                = 16,
;;     ICON_FILE_CUT                 = 17,
;;     ICON_FILE_PASTE               = 18,
;;     ICON_CURSOR_HAND              = 19,
;;     ICON_CURSOR_POINTER           = 20,
;;     ICON_CURSOR_CLASSIC           = 21,
;;     ICON_PENCIL                   = 22,
;;     ICON_PENCIL_BIG               = 23,
;;     ICON_BRUSH_CLASSIC            = 24,
;;     ICON_BRUSH_PAINTER            = 25,
;;     ICON_WATER_DROP               = 26,
;;     ICON_COLOR_PICKER             = 27,
;;     ICON_RUBBER                   = 28,
;;     ICON_COLOR_BUCKET             = 29,
;;     ICON_TEXT_T                   = 30,
;;     ICON_TEXT_A                   = 31,
;;     ICON_SCALE                    = 32,
;;     ICON_RESIZE                   = 33,
;;     ICON_FILTER_POINT             = 34,
;;     ICON_FILTER_BILINEAR          = 35,
;;     ICON_CROP                     = 36,
;;     ICON_CROP_ALPHA               = 37,
;;     ICON_SQUARE_TOGGLE            = 38,
;;     ICON_SYMMETRY                 = 39,
;;     ICON_SYMMETRY_HORIZONTAL      = 40,
;;     ICON_SYMMETRY_VERTICAL        = 41,
;;     ICON_LENS                     = 42,
;;     ICON_LENS_BIG                 = 43,
;;     ICON_EYE_ON                   = 44,
;;     ICON_EYE_OFF                  = 45,
;;     ICON_FILTER_TOP               = 46,
;;     ICON_FILTER                   = 47,
;;     ICON_TARGET_POINT             = 48,
;;     ICON_TARGET_SMALL             = 49,
;;     ICON_TARGET_BIG               = 50,
;;     ICON_TARGET_MOVE              = 51,
;;     ICON_CURSOR_MOVE              = 52,
;;     ICON_CURSOR_SCALE             = 53,
;;     ICON_CURSOR_SCALE_RIGHT       = 54,
;;     ICON_CURSOR_SCALE_LEFT        = 55,
;;     ICON_UNDO                     = 56,
;;     ICON_REDO                     = 57,
;;     ICON_REREDO                   = 58,
;;     ICON_MUTATE                   = 59,
;;     ICON_ROTATE                   = 60,
;;     ICON_REPEAT                   = 61,
;;     ICON_SHUFFLE                  = 62,
;;     ICON_EMPTYBOX                 = 63,
;;     ICON_TARGET                   = 64,
;;     ICON_TARGET_SMALL_FILL        = 65,
;;     ICON_TARGET_BIG_FILL          = 66,
;;     ICON_TARGET_MOVE_FILL         = 67,
;;     ICON_CURSOR_MOVE_FILL         = 68,
;;     ICON_CURSOR_SCALE_FILL        = 69,
;;     ICON_CURSOR_SCALE_RIGHT_FILL  = 70,
;;     ICON_CURSOR_SCALE_LEFT_FILL   = 71,
;;     ICON_UNDO_FILL                = 72,
;;     ICON_REDO_FILL                = 73,
;;     ICON_REREDO_FILL              = 74,
;;     ICON_MUTATE_FILL              = 75,
;;     ICON_ROTATE_FILL              = 76,
;;     ICON_REPEAT_FILL              = 77,
;;     ICON_SHUFFLE_FILL             = 78,
;;     ICON_EMPTYBOX_SMALL           = 79,
;;     ICON_BOX                      = 80,
;;     ICON_BOX_TOP                  = 81,
;;     ICON_BOX_TOP_RIGHT            = 82,
;;     ICON_BOX_RIGHT                = 83,
;;     ICON_BOX_BOTTOM_RIGHT         = 84,
;;     ICON_BOX_BOTTOM               = 85,
;;     ICON_BOX_BOTTOM_LEFT          = 86,
;;     ICON_BOX_LEFT                 = 87,
;;     ICON_BOX_TOP_LEFT             = 88,
;;     ICON_BOX_CENTER               = 89,
;;     ICON_BOX_CIRCLE_MASK          = 90,
;;     ICON_POT                      = 91,
;;     ICON_ALPHA_MULTIPLY           = 92,
;;     ICON_ALPHA_CLEAR              = 93,
;;     ICON_DITHERING                = 94,
;;     ICON_MIPMAPS                  = 95,
;;     ICON_BOX_GRID                 = 96,
;;     ICON_GRID                     = 97,
;;     ICON_BOX_CORNERS_SMALL        = 98,
;;     ICON_BOX_CORNERS_BIG          = 99,
;;     ICON_FOUR_BOXES               = 100,
;;     ICON_GRID_FILL                = 101,
;;     ICON_BOX_MULTISIZE            = 102,
;;     ICON_ZOOM_SMALL               = 103,
;;     ICON_ZOOM_MEDIUM              = 104,
;;     ICON_ZOOM_BIG                 = 105,
;;     ICON_ZOOM_ALL                 = 106,
;;     ICON_ZOOM_CENTER              = 107,
;;     ICON_BOX_DOTS_SMALL           = 108,
;;     ICON_BOX_DOTS_BIG             = 109,
;;     ICON_BOX_CONCENTRIC           = 110,
;;     ICON_BOX_GRID_BIG             = 111,
;;     ICON_OK_TICK                  = 112,
;;     ICON_CROSS                    = 113,
;;     ICON_ARROW_LEFT               = 114,
;;     ICON_ARROW_RIGHT              = 115,
;;     ICON_ARROW_DOWN               = 116,
;;     ICON_ARROW_UP                 = 117,
;;     ICON_ARROW_LEFT_FILL          = 118,
;;     ICON_ARROW_RIGHT_FILL         = 119,
;;     ICON_ARROW_DOWN_FILL          = 120,
;;     ICON_ARROW_UP_FILL            = 121,
;;     ICON_AUDIO                    = 122,
;;     ICON_FX                       = 123,
;;     ICON_WAVE                     = 124,
;;     ICON_WAVE_SINUS               = 125,
;;     ICON_WAVE_SQUARE              = 126,
;;     ICON_WAVE_TRIANGULAR          = 127,
;;     ICON_CROSS_SMALL              = 128,
;;     ICON_PLAYER_PREVIOUS          = 129,
;;     ICON_PLAYER_PLAY_BACK         = 130,
;;     ICON_PLAYER_PLAY              = 131,
;;     ICON_PLAYER_PAUSE             = 132,
;;     ICON_PLAYER_STOP              = 133,
;;     ICON_PLAYER_NEXT              = 134,
;;     ICON_PLAYER_RECORD            = 135,
;;     ICON_MAGNET                   = 136,
;;     ICON_LOCK_CLOSE               = 137,
;;     ICON_LOCK_OPEN                = 138,
;;     ICON_CLOCK                    = 139,
;;     ICON_TOOLS                    = 140,
;;     ICON_GEAR                     = 141,
;;     ICON_GEAR_BIG                 = 142,
;;     ICON_BIN                      = 143,
;;     ICON_HAND_POINTER             = 144,
;;     ICON_LASER                    = 145,
;;     ICON_COIN                     = 146,
;;     ICON_EXPLOSION                = 147,
;;     ICON_1UP                      = 148,
;;     ICON_PLAYER                   = 149,
;;     ICON_PLAYER_JUMP              = 150,
;;     ICON_KEY                      = 151,
;;     ICON_DEMON                    = 152,
;;     ICON_TEXT_POPUP               = 153,
;;     ICON_GEAR_EX                  = 154,
;;     ICON_CRACK                    = 155,
;;     ICON_CRACK_POINTS             = 156,
;;     ICON_STAR                     = 157,
;;     ICON_DOOR                     = 158,
;;     ICON_EXIT                     = 159,
;;     ICON_MODE_2D                  = 160,
;;     ICON_MODE_3D                  = 161,
;;     ICON_CUBE                     = 162,
;;     ICON_CUBE_FACE_TOP            = 163,
;;     ICON_CUBE_FACE_LEFT           = 164,
;;     ICON_CUBE_FACE_FRONT          = 165,
;;     ICON_CUBE_FACE_BOTTOM         = 166,
;;     ICON_CUBE_FACE_RIGHT          = 167,
;;     ICON_CUBE_FACE_BACK           = 168,
;;     ICON_CAMERA                   = 169,
;;     ICON_SPECIAL                  = 170,
;;     ICON_LINK_NET                 = 171,
;;     ICON_LINK_BOXES               = 172,
;;     ICON_LINK_MULTI               = 173,
;;     ICON_LINK                     = 174,
;;     ICON_LINK_BROKE               = 175,
;;     ICON_TEXT_NOTES               = 176,
;;     ICON_NOTEBOOK                 = 177,
;;     ICON_SUITCASE                 = 178,
;;     ICON_SUITCASE_ZIP             = 179,
;;     ICON_MAILBOX                  = 180,
;;     ICON_MONITOR                  = 181,
;;     ICON_PRINTER                  = 182,
;;     ICON_PHOTO_CAMERA             = 183,
;;     ICON_PHOTO_CAMERA_FLASH       = 184,
;;     ICON_HOUSE                    = 185,
;;     ICON_HEART                    = 186,
;;     ICON_CORNER                   = 187,
;;     ICON_VERTICAL_BARS            = 188,
;;     ICON_VERTICAL_BARS_FILL       = 189,
;;     ICON_LIFE_BARS                = 190,
;;     ICON_INFO                     = 191,
;;     ICON_CROSSLINE                = 192,
;;     ICON_HELP                     = 193,
;;     ICON_FILETYPE_ALPHA           = 194,
;;     ICON_FILETYPE_HOME            = 195,
;;     ICON_LAYERS_VISIBLE           = 196,
;;     ICON_LAYERS                   = 197,
;;     ICON_WINDOW                   = 198,
;;     ICON_HIDPI                    = 199,
;;     ICON_FILETYPE_BINARY          = 200,
;;     ICON_HEX                      = 201,
;;     ICON_SHIELD                   = 202,
;;     ICON_FILE_NEW                 = 203,
;;     ICON_FOLDER_ADD               = 204,
;;     ICON_ALARM                    = 205,
;;     ICON_CPU                      = 206,
;;     ICON_ROM                      = 207,
;;     ICON_STEP_OVER                = 208,
;;     ICON_STEP_INTO                = 209,
;;     ICON_STEP_OUT                 = 210,
;;     ICON_RESTART                  = 211,
;;     ICON_BREAKPOINT_ON            = 212,
;;     ICON_BREAKPOINT_OFF           = 213,
;;     ICON_BURGER_MENU              = 214,
;;     ICON_CASE_SENSITIVE           = 215,
;;     ICON_REG_EXP                  = 216,
;;     ICON_FOLDER                   = 217,
;;     ICON_FILE                     = 218,
;;     ICON_219                      = 219,
;;     ICON_220                      = 220,
;;     ICON_221                      = 221,
;;     ICON_222                      = 222,
;;     ICON_223                      = 223,
;;     ICON_224                      = 224,
;;     ICON_225                      = 225,
;;     ICON_226                      = 226,
;;     ICON_227                      = 227,
;;     ICON_228                      = 228,
;;     ICON_229                      = 229,
;;     ICON_230                      = 230,
;;     ICON_231                      = 231,
;;     ICON_232                      = 232,
;;     ICON_233                      = 233,
;;     ICON_234                      = 234,
;;     ICON_235                      = 235,
;;     ICON_236                      = 236,
;;     ICON_237                      = 237,
;;     ICON_238                      = 238,
;;     ICON_239                      = 239,
;;     ICON_240                      = 240,
;;     ICON_241                      = 241,
;;     ICON_242                      = 242,
;;     ICON_243                      = 243,
;;     ICON_244                      = 244,
;;     ICON_245                      = 245,
;;     ICON_246                      = 246,
;;     ICON_247                      = 247,
;;     ICON_248                      = 248,
;;     ICON_249                      = 249,
;;     ICON_250                      = 250,
;;     ICON_251                      = 251,
;;     ICON_252                      = 252,
;;     ICON_253                      = 253,
;;     ICON_254                      = 254,
;;     ICON_255                      = 255,
;; } GuiIconName;
;; #endif

;; I didn't do this by hand btw
(defcenum gui-icon-name
  (:icon-none 0) 
  (:icon-folder-file-open 1) 
  (:icon-file-save-classic 2) 
  (:icon-folder-open 3) 
  (:icon-folder-save 4) 
  (:icon-file-open 5) 
  (:icon-file-save 6) 
  (:icon-file-export 7) 
  (:icon-file-add 8) 
  (:icon-file-delete 9) 
  (:icon-filetype-text 10) 
  (:icon-filetype-audio 11) 
  (:icon-filetype-image 12) 
  (:icon-filetype-play 13) 
  (:icon-filetype-video 14) 
  (:icon-filetype-info 15) 
  (:icon-file-copy 16) 
  (:icon-file-cut 17) 
  (:icon-file-paste 18) 
  (:icon-cursor-hand 19) 
  (:icon-cursor-pointer 20) 
  (:icon-cursor-classic 21) 
  (:icon-pencil 22) 
  (:icon-pencil-big 23) 
  (:icon-brush-classic 24) 
  (:icon-brush-painter 25) 
  (:icon-water-drop 26) 
  (:icon-color-picker 27) 
  (:icon-rubber 28) 
  (:icon-color-bucket 29) 
  (:icon-text-t 30) 
  (:icon-text-a 31) 
  (:icon-scale 32) 
  (:icon-resize 33) 
  (:icon-filter-point 34) 
  (:icon-filter-bilinear 35) 
  (:icon-crop 36) 
  (:icon-crop-alpha 37) 
  (:icon-square-toggle 38) 
  (:icon-symmetry 39) 
  (:icon-symmetry-horizontal 40) 
  (:icon-symmetry-vertical 41) 
  (:icon-lens 42) 
  (:icon-lens-big 43) 
  (:icon-eye-on 44) 
  (:icon-eye-off 45) 
  (:icon-filter-top 46) 
  (:icon-filter 47) 
  (:icon-target-point 48) 
  (:icon-target-small 49) 
  (:icon-target-big 50) 
  (:icon-target-move 51) 
  (:icon-cursor-move 52) 
  (:icon-cursor-scale 53) 
  (:icon-cursor-scale-right 54) 
  (:icon-cursor-scale-left 55) 
  (:icon-undo 56) 
  (:icon-redo 57) 
  (:icon-reredo 58) 
  (:icon-mutate 59) 
  (:icon-rotate 60) 
  (:icon-repeat 61) 
  (:icon-shuffle 62) 
  (:icon-emptybox 63) 
  (:icon-target 64) 
  (:icon-target-small-fill 65) 
  (:icon-target-big-fill 66) 
  (:icon-target-move-fill 67) 
  (:icon-cursor-move-fill 68) 
  (:icon-cursor-scale-fill 69) 
  (:icon-cursor-scale-right-fill 70) 
  (:icon-cursor-scale-left-fill 71) 
  (:icon-undo-fill 72) 
  (:icon-redo-fill 73) 
  (:icon-reredo-fill 74) 
  (:icon-mutate-fill 75) 
  (:icon-rotate-fill 76) 
  (:icon-repeat-fill 77) 
  (:icon-shuffle-fill 78) 
  (:icon-emptybox-small 79) 
  (:icon-box 80) 
  (:icon-box-top 81) 
  (:icon-box-top-right 82) 
  (:icon-box-right 83) 
  (:icon-box-bottom-right 84) 
  (:icon-box-bottom 85) 
  (:icon-box-bottom-left 86) 
  (:icon-box-left 87) 
  (:icon-box-top-left 88) 
  (:icon-box-center 89) 
  (:icon-box-circle-mask 90) 
  (:icon-pot 91) 
  (:icon-alpha-multiply 92) 
  (:icon-alpha-clear 93) 
  (:icon-dithering 94) 
  (:icon-mipmaps 95) 
  (:icon-box-grid 96) 
  (:icon-grid 97) 
  (:icon-box-corners-small 98) 
  (:icon-box-corners-big 99) 
  (:icon-four-boxes 100) 
  (:icon-grid-fill 101) 
  (:icon-box-multisize 102) 
  (:icon-zoom-small 103) 
  (:icon-zoom-medium 104) 
  (:icon-zoom-big 105) 
  (:icon-zoom-all 106) 
  (:icon-zoom-center 107) 
  (:icon-box-dots-small 108) 
  (:icon-box-dots-big 109) 
  (:icon-box-concentric 110) 
  (:icon-box-grid-big 111) 
  (:icon-ok-tick 112) 
  (:icon-cross 113) 
  (:icon-arrow-left 114) 
  (:icon-arrow-right 115) 
  (:icon-arrow-down 116) 
  (:icon-arrow-up 117) 
  (:icon-arrow-left-fill 118) 
  (:icon-arrow-right-fill 119) 
  (:icon-arrow-down-fill 120) 
  (:icon-arrow-up-fill 121) 
  (:icon-audio 122) 
  (:icon-fx 123) 
  (:icon-wave 124) 
  (:icon-wave-sinus 125) 
  (:icon-wave-square 126) 
  (:icon-wave-triangular 127) 
  (:icon-cross-small 128) 
  (:icon-player-previous 129) 
  (:icon-player-play-back 130) 
  (:icon-player-play 131) 
  (:icon-player-pause 132) 
  (:icon-player-stop 133) 
  (:icon-player-next 134) 
  (:icon-player-record 135) 
  (:icon-magnet 136) 
  (:icon-lock-close 137) 
  (:icon-lock-open 138) 
  (:icon-clock 139) 
  (:icon-tools 140) 
  (:icon-gear 141) 
  (:icon-gear-big 142) 
  (:icon-bin 143) 
  (:icon-hand-pointer 144) 
  (:icon-laser 145) 
  (:icon-coin 146) 
  (:icon-explosion 147) 
  (:icon-1up 148) 
  (:icon-player 149) 
  (:icon-player-jump 150) 
  (:icon-key 151) 
  (:icon-demon 152) 
  (:icon-text-popup 153) 
  (:icon-gear-ex 154) 
  (:icon-crack 155) 
  (:icon-crack-points 156) 
  (:icon-star 157) 
  (:icon-door 158) 
  (:icon-exit 159) 
  (:icon-mode-2d 160) 
  (:icon-mode-3d 161) 
  (:icon-cube 162) 
  (:icon-cube-face-top 163) 
  (:icon-cube-face-left 164) 
  (:icon-cube-face-front 165) 
  (:icon-cube-face-bottom 166) 
  (:icon-cube-face-right 167) 
  (:icon-cube-face-back 168) 
  (:icon-camera 169) 
  (:icon-special 170) 
  (:icon-link-net 171) 
  (:icon-link-boxes 172) 
  (:icon-link-multi 173) 
  (:icon-link 174) 
  (:icon-link-broke 175) 
  (:icon-text-notes 176) 
  (:icon-notebook 177) 
  (:icon-suitcase 178) 
  (:icon-suitcase-zip 179) 
  (:icon-mailbox 180) 
  (:icon-monitor 181) 
  (:icon-printer 182) 
  (:icon-photo-camera 183) 
  (:icon-photo-camera-flash 184) 
  (:icon-house 185) 
  (:icon-heart 186) 
  (:icon-corner 187) 
  (:icon-vertical-bars 188) 
  (:icon-vertical-bars-fill 189) 
  (:icon-life-bars 190) 
  (:icon-info 191) 
  (:icon-crossline 192) 
  (:icon-help 193) 
  (:icon-filetype-alpha 194) 
  (:icon-filetype-home 195) 
  (:icon-layers-visible 196) 
  (:icon-layers 197) 
  (:icon-window 198) 
  (:icon-hidpi 199) 
  (:icon-filetype-binary 200) 
  (:icon-hex 201) 
  (:icon-shield 202) 
  (:icon-file-new 203) 
  (:icon-folder-add 204) 
  (:icon-alarm 205) 
  (:icon-cpu 206) 
  (:icon-rom 207) 
  (:icon-step-over 208) 
  (:icon-step-into 209) 
  (:icon-step-out 210) 
  (:icon-restart 211) 
  (:icon-breakpoint-on 212) 
  (:icon-breakpoint-off 213) 
  (:icon-burger-menu 214) 
  (:icon-case-sensitive 215) 
  (:icon-reg-exp 216) 
  (:icon-folder 217) 
  (:icon-file 218) 
  (:icon-219 219) 
  (:icon-220 220) 
  (:icon-221 221) 
  (:icon-222 222) 
  (:icon-223 223) 
  (:icon-224 224) 
  (:icon-225 225) 
  (:icon-226 226) 
  (:icon-227 227) 
  (:icon-228 228) 
  (:icon-229 229) 
  (:icon-230 230) 
  (:icon-231 231) 
  (:icon-232 232) 
  (:icon-233 233) 
  (:icon-234 234) 
  (:icon-235 235) 
  (:icon-236 236) 
  (:icon-237 237) 
  (:icon-238 238) 
  (:icon-239 239) 
  (:icon-240 240) 
  (:icon-241 241) 
  (:icon-242 242) 
  (:icon-243 243) 
  (:icon-244 244) 
  (:icon-245 245) 
  (:icon-246 246) 
  (:icon-247 247) 
  (:icon-248 248) 
  (:icon-249 249) 
  (:icon-250 250) 
  (:icon-251 251) 
  (:icon-252 252) 
  (:icon-253 253) 
  (:icon-254 254) 
  (:icon-255 255))
