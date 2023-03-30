(in-package #:cl-raygui)
;; /*******************************************************************************************
;; *
;; *   raygui v3.2 - A simple and easy-to-use immediate-mode gui library
;; *
;; *   DESCRIPTION:
;; *
;; *   raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also
;; *   available as a standalone library, as long as input and drawing functions are provided.
;; *
;; *   Controls provided:
;; *
;; *   # Container/separators Controls
;; *       - WindowBox     --> StatusBar, Panel
;; *       - GroupBox      --> Line
;; *       - Line
;; *       - Panel         --> StatusBar
;; *       - ScrollPanel   --> StatusBar
;; *
;; *   # Basic Controls
;; *       - Label
;; *       - Button
;; *       - LabelButton   --> Label
;; *       - Toggle
;; *       - ToggleGroup   --> Toggle
;; *       - CheckBox
;; *       - ComboBox
;; *       - DropdownBox
;; *       - TextBox
;; *       - TextBoxMulti
;; *       - ValueBox      --> TextBox
;; *       - Spinner       --> Button, ValueBox
;; *       - Slider
;; *       - SliderBar     --> Slider
;; *       - ProgressBar
;; *       - StatusBar
;; *       - DummyRec
;; *       - Grid
;; *
;; *   # Advance Controls
;; *       - ListView
;; *       - ColorPicker   --> ColorPanel, ColorBarHue
;; *       - MessageBox    --> Window, Label, Button
;; *       - TextInputBox  --> Window, Label, TextBox, Button
;; *
;; *   It also provides a set of functions for styling the controls based on its properties (size, color).
;; *
;; *
;; *   RAYGUI STYLE (guiStyle):
;; *
;; *   raygui uses a global data array for all gui style properties (allocated on data segment by default),
;; *   when a new style is loaded, it is loaded over the global style... but a default gui style could always be
;; *   recovered with GuiLoadStyleDefault() function, that overwrites the current style to the default one
;; *
;; *   The global style array size is fixed and depends on the number of controls and properties:
;; *
;; *       static unsigned int guiStyle[RAYGUI_MAX_CONTROLS*(RAYGUI_MAX_PROPS_BASE + RAYGUI_MAX_PROPS_EXTENDED)];
;; *
;; *   guiStyle size is by default: 16*(16 + 8) = 384*4 = 1536 bytes = 1.5 KB
;; *
;; *   Note that the first set of BASE properties (by default guiStyle[0..15]) belong to the generic style
;; *   used for all controls, when any of those base values is set, it is automatically populated to all
;; *   controls, so, specific control values overwriting generic style should be set after base values.
;; *
;; *   After the first BASE set we have the EXTENDED properties (by default guiStyle[16..23]), those
;; *   properties are actually common to all controls and can not be overwritten individually (like BASE ones)
;; *   Some of those properties are: TEXT_SIZE, TEXT_SPACING, LINE_COLOR, BACKGROUND_COLOR
;; *
;; *   Custom control properties can be defined using the EXTENDED properties for each independent control.
;; *
;; *   TOOL: rGuiStyler is a visual tool to customize raygui style.
;; *
;; *
;; *   RAYGUI ICONS (guiIcons):
;; *
;; *   raygui could use a global array containing icons data (allocated on data segment by default),
;; *   a custom icons set could be loaded over this array using GuiLoadIcons(), but loaded icons set
;; *   must be same RAYGUI_ICON_SIZE and no more than RAYGUI_ICON_MAX_ICONS will be loaded
;; *
;; *   Every icon is codified in binary form, using 1 bit per pixel, so, every 16x16 icon
;; *   requires 8 integers (16*16/32) to be stored in memory.
;; *
;; *   When the icon is draw, actually one quad per pixel is drawn if the bit for that pixel is set.
;; *
;; *   The global icons array size is fixed and depends on the number of icons and size:
;; *
;; *       static unsigned int guiIcons[RAYGUI_ICON_MAX_ICONS*RAYGUI_ICON_DATA_ELEMENTS];
;; *
;; *   guiIcons size is by default: 256*(16*16/32) = 2048*4 = 8192 bytes = 8 KB
;; *
;; *   TOOL: rGuiIcons is a visual tool to customize raygui icons.
;; *
;; *
;; *   CONFIGURATION:
;; *
;; *   #define RAYGUI_IMPLEMENTATION
;; *       Generates the implementation of the library into the included file.
;; *       If not defined, the library is in header only mode and can be included in other headers
;; *       or source files without problems. But only ONE file should hold the implementation.
;; *
;; *   #define RAYGUI_STANDALONE
;; *       Avoid raylib.h header inclusion in this file. Data types defined on raylib are defined
;; *       internally in the library and input management and drawing functions must be provided by
;; *       the user (check library implementation for further details).
;; *
;; *   #define RAYGUI_NO_ICONS
;; *       Avoid including embedded ricons data (256 icons, 16x16 pixels, 1-bit per pixel, 2KB)
;; *
;; *   #define RAYGUI_CUSTOM_ICONS
;; *       Includes custom ricons.h header defining a set of custom icons,
;; *       this file can be generated using rGuiIcons tool
;; *
;; *
;; *   VERSIONS HISTORY:
;; *       3.2 (22-May-2022) RENAMED: Some enum values, for unification, avoiding prefixes
;; *                         REMOVED: GuiScrollBar(), only internal
;; *                         REDESIGNED: GuiPanel() to support text parameter
;; *                         REDESIGNED: GuiScrollPanel() to support text parameter
;; *                         REDESIGNED: GuiColorPicker() to support text parameter
;; *                         REDESIGNED: GuiColorPanel() to support text parameter
;; *                         REDESIGNED: GuiColorBarAlpha() to support text parameter
;; *                         REDESIGNED: GuiColorBarHue() to support text parameter
;; *                         REDESIGNED: GuiTextInputBox() to support password
;; *       3.1 (12-Jan-2022) REVIEWED: Default style for consistency (aligned with rGuiLayout v2.5 tool)
;; *                         REVIEWED: GuiLoadStyle() to support compressed font atlas image data and unload previous textures
;; *                         REVIEWED: External icons usage logic
;; *                         REVIEWED: GuiLine() for centered alignment when including text
;; *                         RENAMED: Multiple controls properties definitions to prepend RAYGUI_
;; *                         RENAMED: RICON_ references to RAYGUI_ICON_ for library consistency
;; *                         Projects updated and multiple tweaks
;; *       3.0 (04-Nov-2021) Integrated ricons data to avoid external file
;; *                         REDESIGNED: GuiTextBoxMulti()
;; *                         REMOVED: GuiImageButton*()
;; *                         Multiple minor tweaks and bugs corrected
;; *       2.9 (17-Mar-2021) REMOVED: Tooltip API
;; *       2.8 (03-May-2020) Centralized rectangles drawing to GuiDrawRectangle()
;; *       2.7 (20-Feb-2020) ADDED: Possible tooltips API
;; *       2.6 (09-Sep-2019) ADDED: GuiTextInputBox()
;; *                         REDESIGNED: GuiListView*(), GuiDropdownBox(), GuiSlider*(), GuiProgressBar(), GuiMessageBox()
;; *                         REVIEWED: GuiTextBox(), GuiSpinner(), GuiValueBox(), GuiLoadStyle()
;; *                         Replaced property INNER_PADDING by TEXT_PADDING, renamed some properties
;; *                         ADDED: 8 new custom styles ready to use
;; *                         Multiple minor tweaks and bugs corrected
;; *       2.5 (28-May-2019) Implemented extended GuiTextBox(), GuiValueBox(), GuiSpinner()
;; *       2.3 (29-Apr-2019) ADDED: rIcons auxiliar library and support for it, multiple controls reviewed
;; *                         Refactor all controls drawing mechanism to use control state
;; *       2.2 (05-Feb-2019) ADDED: GuiScrollBar(), GuiScrollPanel(), reviewed GuiListView(), removed Gui*Ex() controls
;; *       2.1 (26-Dec-2018) REDESIGNED: GuiCheckBox(), GuiComboBox(), GuiDropdownBox(), GuiToggleGroup() > Use combined text string
;; *                         REDESIGNED: Style system (breaking change)
;; *       2.0 (08-Nov-2018) ADDED: Support controls guiLock and custom fonts
;; *                         REVIEWED: GuiComboBox(), GuiListView()...
;; *       1.9 (09-Oct-2018) REVIEWED: GuiGrid(), GuiTextBox(), GuiTextBoxMulti(), GuiValueBox()...
;; *       1.8 (01-May-2018) Lot of rework and redesign to align with rGuiStyler and rGuiLayout
;; *       1.5 (21-Jun-2017) Working in an improved styles system
;; *       1.4 (15-Jun-2017) Rewritten all GUI functions (removed useless ones)
;; *       1.3 (12-Jun-2017) Complete redesign of style system
;; *       1.1 (01-Jun-2017) Complete review of the library
;; *       1.0 (07-Jun-2016) Converted to header-only by Ramon Santamaria.
;; *       0.9 (07-Mar-2016) Reviewed and tested by Albert Martos, Ian Eito, Sergio Martinez and Ramon Santamaria.
;; *       0.8 (27-Aug-2015) Initial release. Implemented by Kevin Gato, Daniel Nicolás and Ramon Santamaria.
;; *
;; *
;; *   CONTRIBUTORS:
;; *
;; *       Ramon Santamaria:   Supervision, review, redesign, update and maintenance
;; *       Vlad Adrian:        Complete rewrite of GuiTextBox() to support extended features (2019)
;; *       Sergio Martinez:    Review, testing (2015) and redesign of multiple controls (2018)
;; *       Adria Arranz:       Testing and Implementation of additional controls (2018)
;; *       Jordi Jorba:        Testing and Implementation of additional controls (2018)
;; *       Albert Martos:      Review and testing of the library (2015)
;; *       Ian Eito:           Review and testing of the library (2015)
;; *       Kevin Gato:         Initial implementation of basic components (2014)
;; *       Daniel Nicolas:     Initial implementation of basic components (2014)
;; *
;; *
;; *   LICENSE: zlib/libpng
;; *
;; *   Copyright (c) 2014-2022 Ramon Santamaria (@raysan5)
;; *
;; *   This software is provided "as-is", without any express or implied warranty. In no event
;; *   will the authors be held liable for any damages arising from the use of this software.
;; *
;; *   Permission is granted to anyone to use this software for any purpose, including commercial
;; *   applications, and to alter it and redistribute it freely, subject to the following restrictions:
;; *
;; *     1. The origin of this software must not be misrepresented; you must not claim that you
;; *     wrote the original software. If you use this software in a product, an acknowledgment
;; *     in the product documentation would be appreciated but is not required.
;; *
;; *     2. Altered source versions must be plainly marked as such, and must not be misrepresented
;; *     as being the original software.
;; *
;; *     3. This notice may not be removed or altered from any source distribution.
;; *
;; **********************************************************************************************/

;; #ifndef RAYGUI_H
;; #define RAYGUI_H

;; #define RAYGUI_VERSION  "3.2"

;; #if !defined(RAYGUI_STANDALONE)
;;     #include "raylib.h"
;; #endif

;; // Function specifiers in case library is build/used as a shared library (Windows)
;; // NOTE: Microsoft specifiers to tell compiler that symbols are imported/exported from a .dll
;; #if defined(_WIN32)
;;     #if defined(BUILD_LIBTYPE_SHARED)
;;         #define RAYGUIAPI __declspec(dllexport)     // We are building the library as a Win32 shared library (.dll)
;;     #elif defined(USE_LIBTYPE_SHARED)
;;         #define RAYGUIAPI __declspec(dllimport)     // We are using the library as a Win32 shared library (.dll)
;;     #endif
;; #endif

;; // Function specifiers definition
;; #ifndef RAYGUIAPI
;;     #define RAYGUIAPI       // Functions defined as 'extern' by default (implicit specifiers)
;; #endif

;; //----------------------------------------------------------------------------------
;; // Defines and Macros
;; //----------------------------------------------------------------------------------
;; // Allow custom memory allocators
;; #ifndef RAYGUI_MALLOC
;;     #define RAYGUI_MALLOC(sz)       malloc(sz)
;; #endif
;; #ifndef RAYGUI_CALLOC
;;     #define RAYGUI_CALLOC(n,sz)     calloc(n,sz)
;; #endif
;; #ifndef RAYGUI_FREE
;;     #define RAYGUI_FREE(p)          free(p)
;; #endif

;; // Simple log system to avoid printf() calls if required
;; // NOTE: Avoiding those calls, also avoids const strings memory usage
;; #define RAYGUI_SUPPORT_LOG_INFO
;; #if defined(RAYGUI_SUPPORT_LOG_INFO)
;;   #define RAYGUI_LOG(...)           printf(__VA_ARGS__)
;; #else
;;   #define RAYGUI_LOG(...)
;; #endif

;; //----------------------------------------------------------------------------------
;; // Types and Structures Definition
;; // NOTE: Some types are required for RAYGUI_STANDALONE usage
;; //----------------------------------------------------------------------------------
;; #if defined(RAYGUI_STANDALONE)
;;     #ifndef __cplusplus
;;     // Boolean type
;;         #ifndef true
;;             typedef enum { false, true } bool;
;;         #endif
;;     #endif

;;     // Vector2 type
;;     typedef struct Vector2 {
;;         float x;
;;         float y;
;;     } Vector2;

;;     // Vector3 type                 // -- ConvertHSVtoRGB(), ConvertRGBtoHSV()
;;     typedef struct Vector3 {
;;         float x;
;;         float y;
;;         float z;
;;     } Vector3;

;;     // Color type, RGBA (32bit)
;;     typedef struct Color {
;;         unsigned char r;
;;         unsigned char g;
;;         unsigned char b;
;;         unsigned char a;
;;     } Color;

;;     // Rectangle type
;;     typedef struct Rectangle {
;;         float x;
;;         float y;
;;         float width;
;;         float height;
;;     } Rectangle;

;;     // TODO: Texture2D type is very coupled to raylib, required by Font type
;;     // It should be redesigned to be provided by user
;;     typedef struct Texture2D {
;;         unsigned int id;        // OpenGL texture id
;;         int width;              // Texture base width
;;         int height;             // Texture base height
;;         int mipmaps;            // Mipmap levels, 1 by default
;;         int format;             // Data format (PixelFormat type)
;;     } Texture2D;

;;     // GlyphInfo, font characters glyphs info
;;     typedef struct GlyphInfo {
;;         int value;              // Character value (Unicode)
;;         int offsetX;            // Character offset X when drawing
;;         int offsetY;            // Character offset Y when drawing
;;         int advanceX;           // Character advance position X
;;         Image image;            // Character image data
;;     } GlyphInfo;

;;     // TODO: Font type is very coupled to raylib, mostly required by GuiLoadStyle()
;;     // It should be redesigned to be provided by user
;;     typedef struct Font {
;;         int baseSize;           // Base size (default chars height)
;;         int glyphCount;         // Number of characters
;;         Texture2D texture;      // Characters texture atlas
;;         Rectangle *recs;        // Characters rectangles in texture
;;         GlyphInfo *chars;       // Characters info data
;;     } Font;
;; #endif

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

(define-constant +state-normal+ 0)
(define-constant +state-focused+ 1)
(define-constant +state-pressed+ 2)
(define-constant +state-disabled+ 3)

;; // Gui control text alignment
;; typedef enum {
;;     TEXT_ALIGN_LEFT = 0,
;;     TEXT_ALIGN_CENTER,
;;     TEXT_ALIGN_RIGHT,
;; } GuiTextAlignment;

(define-constant +text-align-left+ 0)
(define-constant +text-align-center+ 1)
(define-constant +text-align-right+ 2)

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

(define-constant +default+ 0)
(define-constant +label+ 1)
(define-constant +button+ 2)
(define-constant +toggle+ 3)
(define-constant +slider+ 4)
(define-constant +progressbar+ 5)
(define-constant +checkbox+ 6)
(define-constant +combobox+ 7)
(define-constant +dropdownbox+ 8)
(define-constant +textbox+ 9)
(define-constant +valuebox+ 10)
(define-constant +spinner+ 11)
(define-constant +listview+ 12)
(define-constant +colorpicker+ 13)
(define-constant +scrollbar+ 14)
(define-constant +statusbar+ 15)

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

(define-constant +border-color-normal+ 0)
(define-constant +base-color-normal+ 1)
(define-constant +text-color-normal+ 2)
(define-constant +border-color-focused+ 3)
(define-constant +base-color-focused+ 4)
(define-constant +text-color-focused+ 5)
(define-constant +border-color-pressed+ 6)
(define-constant +base-color-pressed+ 7)
(define-constant +text-color-pressed+ 8)
(define-constant +border-color-disabled+ 9)
(define-constant +base-color-disabled+ 10)
(define-constant +text-color-disabled+ 11)
(define-constant +border-width+ 12)
(define-constant +text-padding+ 13)
(define-constant +text-alignment+ 14)
(define-constant +reserved+ 15)

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

(define-constant +text-size+ 16)
(define-constant +text-spacing+ 17)
(define-constant +line-color+ 18)
(define-constant +background-color+ 19)

;; // Label
;; //typedef enum { } GuiLabelProperty;

;; // Button/Spinner
;; //typedef enum { } GuiButtonProperty;

;; // Toggle/ToggleGroup
;; typedef enum {
;;     GROUP_PADDING = 16,         // ToggleGroup separation between toggles
;; } GuiToggleProperty;

(define-constant +group-padding+ 16)

;; // Slider/SliderBar
;; typedef enum {
;;     SLIDER_WIDTH = 16,          // Slider size of internal bar
;;     SLIDER_PADDING              // Slider/SliderBar internal bar padding
;; } GuiSliderProperty;

(define-constant +slider-width+ 16)
(define-constant +slider-padding+ 17)

;; // ProgressBar
;; typedef enum {
;;     PROGRESS_PADDING = 16,      // ProgressBar internal padding
;; } GuiProgressBarProperty;

(define-constant +progress-padding+ 16)

;; // ScrollBar
;; typedef enum {
;;     ARROWS_SIZE = 16,
;;     ARROWS_VISIBLE,
;;     SCROLL_SLIDER_PADDING,      // (SLIDERBAR, SLIDER_PADDING)
;;     SCROLL_SLIDER_SIZE,
;;     SCROLL_PADDING,
;;     SCROLL_SPEED,
;; } GuiScrollBarProperty;

(define-constant +arrows-size+ 16)
(define-constant +arrows-visible+ 17)
(define-constant +scroll-slider-padding+ 18)
(define-constant +scroll-slider-size+ 19)
(define-constant +scroll-padding+ 20)
(define-constant +scroll-speed+ 21)

;; // CheckBox
;; typedef enum {
;;     CHECK_PADDING = 16          // CheckBox internal check padding
;; } GuiCheckBoxProperty;

(define-constant +check-padding+ 16)

;; // ComboBox
;; typedef enum {
;;     COMBO_BUTTON_WIDTH = 16,    // ComboBox right button width
;;     COMBO_BUTTON_SPACING        // ComboBox button separation
;; } GuiComboBoxProperty;

(define-constant +combo-button-width+ 16)
(define-constant +combo-button-spacing+ 17)

;; // DropdownBox
;; typedef enum {
;;     ARROW_PADDING = 16,         // DropdownBox arrow separation from border and items
;;     DROPDOWN_ITEMS_SPACING      // DropdownBox items separation
;; } GuiDropdownBoxProperty;

(define-constant +arrow-padding+ 16)
(define-constant +dropdown-items-spacing+ 17)

;; // TextBox/TextBoxMulti/ValueBox/Spinner
;; typedef enum {
;;     TEXT_INNER_PADDING = 16,    // TextBox/TextBoxMulti/ValueBox/Spinner inner text padding
;;     TEXT_LINES_SPACING,         // TextBoxMulti lines separation
;; } GuiTextBoxProperty;

(define-constant +text-inner-padding+ 16)
(define-constant +text-lines-spacing+ 17)

;; // Spinner
;; typedef enum {
;;     SPIN_BUTTON_WIDTH = 16,     // Spinner left/right buttons width
;;     SPIN_BUTTON_SPACING,        // Spinner buttons separation
;; } GuiSpinnerProperty;

(define-constant +spin-button-width+ 16)
(define-constant +spin-button-spacing+ 17)

;; // ListView
;; typedef enum {
;;     LIST_ITEMS_HEIGHT = 16,     // ListView items height
;;     LIST_ITEMS_SPACING,         // ListView items separation
;;     SCROLLBAR_WIDTH,            // ListView scrollbar size (usually width)
;;     SCROLLBAR_SIDE,             // ListView scrollbar side (0-left, 1-right)
;; } GuiListViewProperty;

(define-constant +list-items-height+ 16)
(define-constant +list-items-spacing+ 17)
(define-constant +scrollbar-width+ 18)
(define-constant +scrollbar-side+ 19)

;; // ColorPicker
;; typedef enum {
;;     COLOR_SELECTOR_SIZE = 16,
;;     HUEBAR_WIDTH,               // ColorPicker right hue bar width
;;     HUEBAR_PADDING,             // ColorPicker right hue bar separation from panel
;;     HUEBAR_SELECTOR_HEIGHT,     // ColorPicker right hue bar selector height
;;     HUEBAR_SELECTOR_OVERFLOW    // ColorPicker right hue bar selector overflow
;; } GuiColorPickerProperty;

(define-constant +color-selector-size+ 16)
(define-constant +huebar-width+ 17)
(define-constant +huebar-padding+ 18)
(define-constant +huebar-selector-height+ 19)
(define-constant +huebar-selector-overflow+ 20)

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
  `(int-bool (%gui-text-box ,bounds ,text ,text-size (bool-int ,edit-mode))))

;; RAYGUIAPI bool GuiTextBoxMulti(Rectangle bounds, char *text, int textSize, bool editMode);              // Text Box control with multiple lines
(defcfun ("GuiTextBoxMulti" %gui-text-box-multi) :int
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (text-size :int)
  (edit-mode :int))

(defmacro gui-text-box-multi (bounds text text-size edit-mode)
  "Text Box control with multiple lines"
  `(int-bool (%gui-text-box-multi ,bounds ,text ,text-size (bool-int ,edit-mode))))

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

;; // Icons functionality
;; RAYGUIAPI const char *GuiIconText(int iconId, const char *text); // Get text with icon id prepended (if supported)
(defcfun "GuiIconText" :string
  "Get text with icon id prepended (if supported)"
  (icon-id :int)
  (text :string))

;; #if !defined(RAYGUI_NO_ICONS)
;; RAYGUIAPI void GuiDrawIcon(int iconId, int posX, int posY, int pixelSize, Color color);
(defcfun "GuiDrawIcon" :void
  (icon-id :int)
  (pos-x :int)
  (pos-y :int)
  (pixel-size :int)
  (color :uint))

;; RAYGUIAPI unsigned int *GuiGetIcons(void);                      // Get full icons data pointer
(defcfun "GuiGetIcons" (:pointer :unsigned-int)
  "Get full icons data pointer")

;; RAYGUIAPI unsigned int *GuiGetIconData(int iconId);             // Get icon bit data
(defcfun "GuiGetIconData" (:pointer :unsigned-int)
  "Get icon bit data"
  (icon-id :int))

;; RAYGUIAPI void GuiSetIconData(int iconId, unsigned int *data);  // Set icon bit data
(defcfun "GuiSetIconData" :void
  "Set icon bit data"
  (icon-id :int)
  (data (:pointer :unsigned-int)))

;; RAYGUIAPI void GuiSetIconScale(unsigned int scale);             // Set icon scale (1 by default)
(defcfun "GuiSetIconScale" :void
  (scale :unsigned-int))

;; RAYGUIAPI void GuiSetIconPixel(int iconId, int x, int y);       // Set icon pixel value
(defcfun "GuiSetIconPixel" :void
  "Set icon pixel value"
  (icon-id :int)
  (x :int)
  (y :int))

;; RAYGUIAPI void GuiClearIconPixel(int iconId, int x, int y);     // Clear icon pixel value
(defcfun "GuiClearIconPixel" :void
  "Clear icon pixel value"
  (icon-id :int)
  (x :int)
  (y :int))

;; RAYGUIAPI bool GuiCheckIconPixel(int iconId, int x, int y);     // Check icon pixel value
(defcfun ("GuiCheckIconPixel" %gui-check-icon-pixel) :int
  (icon-id :int)
  (x :int)
  (y :int))

(defmacro gui-check-icon-pixel (icon-id x y)
  "Check icon pixel value"
  `(int-bool (%gui-check-icon-pixel ,icon-id ,x ,y)))

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
;;     ICON_206                      = 206,
;;     ICON_207                      = 207,
;;     ICON_208                      = 208,
;;     ICON_209                      = 209,
;;     ICON_210                      = 210,
;;     ICON_211                      = 211,
;;     ICON_212                      = 212,
;;     ICON_213                      = 213,
;;     ICON_214                      = 214,
;;     ICON_215                      = 215,
;;     ICON_216                      = 216,
;;     ICON_217                      = 217,
;;     ICON_218                      = 218,
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
(define-constant +icon-none+ 0) 
(define-constant +icon-folder-file-open+ 1) 
(define-constant +icon-file-save-classic+ 2) 
(define-constant +icon-folder-open+ 3) 
(define-constant +icon-folder-save+ 4) 
(define-constant +icon-file-open+ 5) 
(define-constant +icon-file-save+ 6) 
(define-constant +icon-file-export+ 7) 
(define-constant +icon-file-add+ 8) 
(define-constant +icon-file-delete+ 9) 
(define-constant +icon-filetype-text+ 10) 
(define-constant +icon-filetype-audio+ 11) 
(define-constant +icon-filetype-image+ 12) 
(define-constant +icon-filetype-play+ 13) 
(define-constant +icon-filetype-video+ 14) 
(define-constant +icon-filetype-info+ 15) 
(define-constant +icon-file-copy+ 16) 
(define-constant +icon-file-cut+ 17) 
(define-constant +icon-file-paste+ 18) 
(define-constant +icon-cursor-hand+ 19) 
(define-constant +icon-cursor-pointer+ 20) 
(define-constant +icon-cursor-classic+ 21) 
(define-constant +icon-pencil+ 22) 
(define-constant +icon-pencil-big+ 23) 
(define-constant +icon-brush-classic+ 24) 
(define-constant +icon-brush-painter+ 25) 
(define-constant +icon-water-drop+ 26) 
(define-constant +icon-color-picker+ 27) 
(define-constant +icon-rubber+ 28) 
(define-constant +icon-color-bucket+ 29) 
(define-constant +icon-text-t+ 30) 
(define-constant +icon-text-a+ 31) 
(define-constant +icon-scale+ 32) 
(define-constant +icon-resize+ 33) 
(define-constant +icon-filter-point+ 34) 
(define-constant +icon-filter-bilinear+ 35) 
(define-constant +icon-crop+ 36) 
(define-constant +icon-crop-alpha+ 37) 
(define-constant +icon-square-toggle+ 38) 
(define-constant +icon-symmetry+ 39) 
(define-constant +icon-symmetry-horizontal+ 40) 
(define-constant +icon-symmetry-vertical+ 41) 
(define-constant +icon-lens+ 42) 
(define-constant +icon-lens-big+ 43) 
(define-constant +icon-eye-on+ 44) 
(define-constant +icon-eye-off+ 45) 
(define-constant +icon-filter-top+ 46) 
(define-constant +icon-filter+ 47) 
(define-constant +icon-target-point+ 48) 
(define-constant +icon-target-small+ 49) 
(define-constant +icon-target-big+ 50) 
(define-constant +icon-target-move+ 51) 
(define-constant +icon-cursor-move+ 52) 
(define-constant +icon-cursor-scale+ 53) 
(define-constant +icon-cursor-scale-right+ 54) 
(define-constant +icon-cursor-scale-left+ 55) 
(define-constant +icon-undo+ 56) 
(define-constant +icon-redo+ 57) 
(define-constant +icon-reredo+ 58) 
(define-constant +icon-mutate+ 59) 
(define-constant +icon-rotate+ 60) 
(define-constant +icon-repeat+ 61) 
(define-constant +icon-shuffle+ 62) 
(define-constant +icon-emptybox+ 63) 
(define-constant +icon-target+ 64) 
(define-constant +icon-target-small-fill+ 65) 
(define-constant +icon-target-big-fill+ 66) 
(define-constant +icon-target-move-fill+ 67) 
(define-constant +icon-cursor-move-fill+ 68) 
(define-constant +icon-cursor-scale-fill+ 69) 
(define-constant +icon-cursor-scale-right-fill+ 70) 
(define-constant +icon-cursor-scale-left-fill+ 71) 
(define-constant +icon-undo-fill+ 72) 
(define-constant +icon-redo-fill+ 73) 
(define-constant +icon-reredo-fill+ 74) 
(define-constant +icon-mutate-fill+ 75) 
(define-constant +icon-rotate-fill+ 76) 
(define-constant +icon-repeat-fill+ 77) 
(define-constant +icon-shuffle-fill+ 78) 
(define-constant +icon-emptybox-small+ 79) 
(define-constant +icon-box+ 80) 
(define-constant +icon-box-top+ 81) 
(define-constant +icon-box-top-right+ 82) 
(define-constant +icon-box-right+ 83) 
(define-constant +icon-box-bottom-right+ 84) 
(define-constant +icon-box-bottom+ 85) 
(define-constant +icon-box-bottom-left+ 86) 
(define-constant +icon-box-left+ 87) 
(define-constant +icon-box-top-left+ 88) 
(define-constant +icon-box-center+ 89) 
(define-constant +icon-box-circle-mask+ 90) 
(define-constant +icon-pot+ 91) 
(define-constant +icon-alpha-multiply+ 92) 
(define-constant +icon-alpha-clear+ 93) 
(define-constant +icon-dithering+ 94) 
(define-constant +icon-mipmaps+ 95) 
(define-constant +icon-box-grid+ 96) 
(define-constant +icon-grid+ 97) 
(define-constant +icon-box-corners-small+ 98) 
(define-constant +icon-box-corners-big+ 99) 
(define-constant +icon-four-boxes+ 100) 
(define-constant +icon-grid-fill+ 101) 
(define-constant +icon-box-multisize+ 102) 
(define-constant +icon-zoom-small+ 103) 
(define-constant +icon-zoom-medium+ 104) 
(define-constant +icon-zoom-big+ 105) 
(define-constant +icon-zoom-all+ 106) 
(define-constant +icon-zoom-center+ 107) 
(define-constant +icon-box-dots-small+ 108) 
(define-constant +icon-box-dots-big+ 109) 
(define-constant +icon-box-concentric+ 110) 
(define-constant +icon-box-grid-big+ 111) 
(define-constant +icon-ok-tick+ 112) 
(define-constant +icon-cross+ 113) 
(define-constant +icon-arrow-left+ 114) 
(define-constant +icon-arrow-right+ 115) 
(define-constant +icon-arrow-down+ 116) 
(define-constant +icon-arrow-up+ 117) 
(define-constant +icon-arrow-left-fill+ 118) 
(define-constant +icon-arrow-right-fill+ 119) 
(define-constant +icon-arrow-down-fill+ 120) 
(define-constant +icon-arrow-up-fill+ 121) 
(define-constant +icon-audio+ 122) 
(define-constant +icon-fx+ 123) 
(define-constant +icon-wave+ 124) 
(define-constant +icon-wave-sinus+ 125) 
(define-constant +icon-wave-square+ 126) 
(define-constant +icon-wave-triangular+ 127) 
(define-constant +icon-cross-small+ 128) 
(define-constant +icon-player-previous+ 129) 
(define-constant +icon-player-play-back+ 130) 
(define-constant +icon-player-play+ 131) 
(define-constant +icon-player-pause+ 132) 
(define-constant +icon-player-stop+ 133) 
(define-constant +icon-player-next+ 134) 
(define-constant +icon-player-record+ 135) 
(define-constant +icon-magnet+ 136) 
(define-constant +icon-lock-close+ 137) 
(define-constant +icon-lock-open+ 138) 
(define-constant +icon-clock+ 139) 
(define-constant +icon-tools+ 140) 
(define-constant +icon-gear+ 141) 
(define-constant +icon-gear-big+ 142) 
(define-constant +icon-bin+ 143) 
(define-constant +icon-hand-pointer+ 144) 
(define-constant +icon-laser+ 145) 
(define-constant +icon-coin+ 146) 
(define-constant +icon-explosion+ 147) 
(define-constant +icon-1up+ 148) 
(define-constant +icon-player+ 149) 
(define-constant +icon-player-jump+ 150) 
(define-constant +icon-key+ 151) 
(define-constant +icon-demon+ 152) 
(define-constant +icon-text-popup+ 153) 
(define-constant +icon-gear-ex+ 154) 
(define-constant +icon-crack+ 155) 
(define-constant +icon-crack-points+ 156) 
(define-constant +icon-star+ 157) 
(define-constant +icon-door+ 158) 
(define-constant +icon-exit+ 159) 
(define-constant +icon-mode-2d+ 160) 
(define-constant +icon-mode-3d+ 161) 
(define-constant +icon-cube+ 162) 
(define-constant +icon-cube-face-top+ 163) 
(define-constant +icon-cube-face-left+ 164) 
(define-constant +icon-cube-face-front+ 165) 
(define-constant +icon-cube-face-bottom+ 166) 
(define-constant +icon-cube-face-right+ 167) 
(define-constant +icon-cube-face-back+ 168) 
(define-constant +icon-camera+ 169) 
(define-constant +icon-special+ 170) 
(define-constant +icon-link-net+ 171) 
(define-constant +icon-link-boxes+ 172) 
(define-constant +icon-link-multi+ 173) 
(define-constant +icon-link+ 174) 
(define-constant +icon-link-broke+ 175) 
(define-constant +icon-text-notes+ 176) 
(define-constant +icon-notebook+ 177) 
(define-constant +icon-suitcase+ 178) 
(define-constant +icon-suitcase-zip+ 179) 
(define-constant +icon-mailbox+ 180) 
(define-constant +icon-monitor+ 181) 
(define-constant +icon-printer+ 182) 
(define-constant +icon-photo-camera+ 183) 
(define-constant +icon-photo-camera-flash+ 184) 
(define-constant +icon-house+ 185) 
(define-constant +icon-heart+ 186) 
(define-constant +icon-corner+ 187) 
(define-constant +icon-vertical-bars+ 188) 
(define-constant +icon-vertical-bars-fill+ 189) 
(define-constant +icon-life-bars+ 190) 
(define-constant +icon-info+ 191) 
(define-constant +icon-crossline+ 192) 
(define-constant +icon-help+ 193) 
(define-constant +icon-filetype-alpha+ 194) 
(define-constant +icon-filetype-home+ 195) 
(define-constant +icon-layers-visible+ 196) 
(define-constant +icon-layers+ 197) 
(define-constant +icon-window+ 198) 
(define-constant +icon-hidpi+ 199) 
(define-constant +icon-filetype-binary+ 200) 
(define-constant +icon-hex+ 201) 
(define-constant +icon-shield+ 202) 
(define-constant +icon-file-new+ 203) 
(define-constant +icon-folder-add+ 204) 
(define-constant +icon-alarm+ 205) 
(define-constant +icon-206+ 206) 
(define-constant +icon-207+ 207) 
(define-constant +icon-208+ 208) 
(define-constant +icon-209+ 209) 
(define-constant +icon-210+ 210) 
(define-constant +icon-211+ 211) 
(define-constant +icon-212+ 212) 
(define-constant +icon-213+ 213) 
(define-constant +icon-214+ 214) 
(define-constant +icon-215+ 215) 
(define-constant +icon-216+ 216) 
(define-constant +icon-217+ 217) 
(define-constant +icon-218+ 218) 
(define-constant +icon-219+ 219) 
(define-constant +icon-220+ 220) 
(define-constant +icon-221+ 221) 
(define-constant +icon-222+ 222) 
(define-constant +icon-223+ 223) 
(define-constant +icon-224+ 224) 
(define-constant +icon-225+ 225) 
(define-constant +icon-226+ 226) 
(define-constant +icon-227+ 227) 
(define-constant +icon-228+ 228) 
(define-constant +icon-229+ 229) 
(define-constant +icon-230+ 230) 
(define-constant +icon-231+ 231) 
(define-constant +icon-232+ 232) 
(define-constant +icon-233+ 233) 
(define-constant +icon-234+ 234) 
(define-constant +icon-235+ 235) 
(define-constant +icon-236+ 236) 
(define-constant +icon-237+ 237) 
(define-constant +icon-238+ 238) 
(define-constant +icon-239+ 239) 
(define-constant +icon-240+ 240) 
(define-constant +icon-241+ 241) 
(define-constant +icon-242+ 242) 
(define-constant +icon-243+ 243) 
(define-constant +icon-244+ 244) 
(define-constant +icon-245+ 245) 
(define-constant +icon-246+ 246) 
(define-constant +icon-247+ 247) 
(define-constant +icon-248+ 248) 
(define-constant +icon-249+ 249) 
(define-constant +icon-250+ 250) 
(define-constant +icon-251+ 251) 
(define-constant +icon-252+ 252) 
(define-constant +icon-253+ 253) 
(define-constant +icon-254+ 254) 
(define-constant +icon-255+ 255)

;; #endif

;; #if defined(__cplusplus)
;; }            // Prevents name mangling of functions
;; #endif

;; #endif // RAYGUI_H
