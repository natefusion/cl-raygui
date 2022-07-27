(in-package #:cl-raygui)
;; /*******************************************************************************************
;; *
;; *   raygui v3.1 - A simple and easy-to-use immediate-mode gui library
;; *
;; *   DESCRIPTION:
;; *
;; *   raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also
;; *   available as a standalone library, as long as input and drawing functions are provided.
;; *
;; *   Controls provided:
;; *
;; *   # Container/separators Controls
;; *       - WindowBox
;; *       - GroupBox
;; *       - Line
;; *       - Panel
;; *       - ScrollPanel
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
;; *       - ScrollBar     // TODO: Really? Do we need it? We have GuiScrollPanel()
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
;; *       3.1 (12-Jan-2021) REVIEWED: Default style for consistency (aligned with rGuiLayout v2.5 tool)
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

;; #define RAYGUI_VERSION  "3.1"

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
;;   #define RAYGUI_LOG(...) printf(__VA_ARGS__)
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
;;     int propertyValue;
;; } GuiStyleProp;

(defcstruct (%gui-style-prop :class gui-style-prop-type)
  "Style property"
  (control-id :unsigned-short)
  (property-id :unsigned-short)
  (property-value :unsigned-short))

(defstruct gui-style-property
  control-id property-id property-value)

(defmethod translate-into-foreign-memory (object (type gui-style-prop-type) pointer)
  (with-foreign-slots ((control-id property-id property-value) pointer (:struct %gui-style-prop))
    (setf control-id (coerce (gui-style-prop-control-id object) 'unsigned-short))
    (setf property-id (coerce (gui-style-prop-property-id object) 'unsigned-short))
    (setf property-value (coerce (gui-style-prop-property-value object) 'unsigned-short))))

(defmethod translate-from-foreign (pointer (type gui-style-prop-type))
  (with-foreign-slots ((control-id property-id property-value) pointer (:struct %gui-style-prop))
    (make-gui-style-prop :control-id control-id :property-id property-id :property-value property-value)))


;; // Gui control state
;; typedef enum {
;;     GUI_STATE_NORMAL = 0,
;;     GUI_STATE_FOCUSED,
;;     GUI_STATE_PRESSED,
;;     GUI_STATE_DISABLED,
;; } GuiControlState;

(define-constant +gui-state-normal+ 0)
(define-constant +gui-state-focused+ 1)
(define-constant +gui-state-pressed+ 2)
(define-constant +gui-state-disabled+ 3)

;; // Gui control text alignment
;; typedef enum {
;;     GUI_TEXT_ALIGN_LEFT = 0,
;;     GUI_TEXT_ALIGN_CENTER,
;;     GUI_TEXT_ALIGN_RIGHT,
;; } GuiTextAlignment;

(define-constant +gui-text-align-left+ 0)
(define-constant +gui-text-align-center+ 1)
(define-constant +gui-text-align-right+ 2)

;; // Gui controls
;; typedef enum {
;;     DEFAULT = 0,    // Generic control -> populates to all controls when set
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
;;     SPINNER,
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

;; // DEFAULT extended properties
;; // NOTE: Those properties are actually common to all controls
;; typedef enum {
;;     TEXT_SIZE = 16,
;;     TEXT_SPACING,
;;     LINE_COLOR,
;;     BACKGROUND_COLOR,
;; } GuiDefaultProperty;

(define-constant +text-size+ 16)
(define-constant +text-spacing+ 17)
(define-constant +line-color+ 18)
(define-constant +background-color+ 19)

;; // Toggle/ToggleGroup
;; typedef enum {
;;     GROUP_PADDING = 16,
;; } GuiToggleProperty;

(define-constant +group-padding+ 16)

;; // Slider/SliderBar
;; typedef enum {
;;     SLIDER_WIDTH = 16,
;;     SLIDER_PADDING
;; } GuiSliderProperty;

(define-constant +slider-width+ 16)
(define-constant +slider-padding+ 17)

;; // ProgressBar
;; typedef enum {
;;     PROGRESS_PADDING = 16,
;; } GuiProgressBarProperty;

(define-constant +progress-padding+ 16)

;; // CheckBox
;; typedef enum {
;;     CHECK_PADDING = 16
;; } GuiCheckBoxProperty;

(define-constant +check-padding+ 16)

;; // ComboBox
;; typedef enum {
;;     COMBO_BUTTON_WIDTH = 16,
;;     COMBO_BUTTON_PADDING
;; } GuiComboBoxProperty;

(define-constant +combo-button-width+ 16)
(define-constant +combo-button-padding+ 17)

;; // DropdownBox
;; typedef enum {
;;     ARROW_PADDING = 16,
;;     DROPDOWN_ITEMS_PADDING
;; } GuiDropdownBoxProperty;

(define-constant +arrow-padding+ 16)
(define-constant +dropdown-items-padding+ 17)

;; // TextBox/TextBoxMulti/ValueBox/Spinner
;; typedef enum {
;;     TEXT_INNER_PADDING = 16,
;;     TEXT_LINES_PADDING,
;;     COLOR_SELECTED_FG,
;;     COLOR_SELECTED_BG
;; } GuiTextBoxProperty;

(define-constant +text-inner-padding+ 16)
(define-constant +text-lines-padding+ 17)
(define-constant +color-seleceted-fg+ 18)
(define-constant +color-selected-bg+ 19)

;; // Spinner
;; typedef enum {
;;     SPIN_BUTTON_WIDTH = 16,
;;     SPIN_BUTTON_PADDING,
;; } GuiSpinnerProperty;

(define-constant +spin-button-width+ 16)
(define-constant +spin-button-padding+ 17)

;; // ScrollBar
;; typedef enum {
;;     ARROWS_SIZE = 16,
;;     ARROWS_VISIBLE,
;;     SCROLL_SLIDER_PADDING,
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


;; // ScrollBar side
;; typedef enum {
;;     SCROLLBAR_LEFT_SIDE = 0,
;;     SCROLLBAR_RIGHT_SIDE
;; } GuiScrollBarSide;

(define-constant +scrollbar-left-side+ 0)
(define-constant +scrollbar-right-side+ 1)

;; // ListView
;; typedef enum {
;;     LIST_ITEMS_HEIGHT = 16,
;;     LIST_ITEMS_PADDING,
;;     SCROLLBAR_WIDTH,
;;     SCROLLBAR_SIDE,
;; } GuiListViewProperty;

(define-constant +list-items-height+ 16)
(define-constant +list-items-padding+ 17)
(define-constant +scrollbar-width+ 18)
(define-constant +scrollbar-side+ 19)

;; // ColorPicker
;; typedef enum {
;;     COLOR_SELECTOR_SIZE = 16,
;;     HUEBAR_WIDTH,                  // Right hue bar width
;;     HUEBAR_PADDING,                // Right hue bar separation from panel
;;     HUEBAR_SELECTOR_HEIGHT,        // Right hue bar selector height
;;     HUEBAR_SELECTOR_OVERFLOW       // Right hue bar selector overflow
;; } GuiColorPickerProperty;

(define-constant +color-selector-size+ 16)
(define-constant +huebar-width+ 17)
(define-constant +huebar-padding+ 18)
(define-constant +huebar-selector-height+ 19)
(define-constant +huebar-selector-overflow+ 20)

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

;; #if defined(__cplusplus)
;; extern "C" {            // Prevents name mangling of functions
;; #endif

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
(defcfun "GuiIsLocked" :bool
  "Check if gui is locked (global state)")

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
(defcfun "GuiWindowBox" :bool
  "Window Box control, shows a window that can be closed"
  (bounds (:struct cl-raylib::%rectangle))
  (title :string))

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

;; RAYGUIAPI void GuiPanel(Rectangle bounds);                                                              // Panel control, useful to group controls
(defcfun "GuiPanel" :void
  "Panel control, useful to group controls"
  (bounds (:struct cl-raylib::%rectangle)))

;; RAYGUIAPI Rectangle GuiScrollPanel(Rectangle bounds, Rectangle content, Vector2 *scroll);               // Scroll Panel control
(defcfun "GuiScrollPanel" (:struct cl-raylib::%rectangle)
  "Scroll panel control"
  (bounds (:struct cl-raylib::%rectangle))
  (content (:struct cl-raylib::%rectangle))
  (scroll (:pointer (:struct cl-raylib::%vector2))))

;; // Basic controls set
;; RAYGUIAPI void GuiLabel(Rectangle bounds, const char *text);                                            // Label control, shows text
(defcfun "GuiLabel" :void
  "Labl control, shows text"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI bool GuiButton(Rectangle bounds, const char *text);                                           // Button control, returns true when clicked
(defcfun "GuiButton" :bool
  "Button control, returns true when clicked"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI bool GuiLabelButton(Rectangle bounds, const char *text);                                      // Label button control, show true when clicked
(defcfun "GuiLabelButton" :bool
  "Label button control, show true when clicked"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string))

;; RAYGUIAPI bool GuiToggle(Rectangle bounds, const char *text, bool active);                              // Toggle Button control, returns true when active
(defcfun "GuiToggle" :bool
  "Toggle button control, returns true when active"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active :bool))

;; RAYGUIAPI int GuiToggleGroup(Rectangle bounds, const char *text, int active);                           // Toggle Group control, returns active toggle index
(defcfun "GuiToggleGroup" :int
  "Toggle Group control, returns active toggle index"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active :int))

;; RAYGUIAPI bool GuiCheckBox(Rectangle bounds, const char *text, bool checked);                           // Check Box control, returns true when active
(defcfun "GuiCheckBox" :bool
  "Check Box control, returns true when active"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (checked :bool))

;; RAYGUIAPI int GuiComboBox(Rectangle bounds, const char *text, int active);                              // Combo Box control, returns selected item index
(defcfun "GuiComboBox" :int
  "Combo Box control, returns selected item index"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active :int))

;; RAYGUIAPI bool GuiDropdownBox(Rectangle bounds, const char *text, int *active, bool editMode);          // Dropdown Box control, returns selected item
(defcfun "GuiDropdownBox" :bool
  "Dropdown Box control, returns selected item"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (active (:pointer :int))
  (edit-mode :bool))

;; RAYGUIAPI bool GuiSpinner(Rectangle bounds, const char *text, int *value, int minValue, int maxValue, bool editMode);     // Spinner control, returns selected value
(defcfun "GuiSpinner" :bool
  "Spinner control, returns selected value"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (value (:pointer :int))
  (min-value :int)
  (max-value :int)
  (edit-mode :bool))

;; RAYGUIAPI bool GuiValueBox(Rectangle bounds, const char *text, int *value, int minValue, int maxValue, bool editMode);    // Value Box control, updates input text with numbers
(defcfun "GuiValueBox" :bool
  "Value Box control, updates input text with numbers"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (value (:pointer :int))
  (min-value :int)
  (max-value :int)
  (edit-mode :bool))

;; RAYGUIAPI bool GuiTextBox(Rectangle bounds, char *text, int textSize, bool editMode);                   // Text Box control, updates input text
(defcfun "GuiTextBox" :bool
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (text-size :int)
  (edit-mode :bool))

;; RAYGUIAPI bool GuiTextBoxMulti(Rectangle bounds, char *text, int textSize, bool editMode);              // Text Box control with multiple lines
(defcfun "GuiTextBoxMulti" :bool
  "Text Box control with multiple lines"
  (bounds (:struct cl-raylib::%rectangle))
  (text :string)
  (text-size :int)
  (edit-mode :bool))

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
  "Slidef bar control, returns selected value"
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

;; RAYGUIAPI int GuiScrollBar(Rectangle bounds, int value, int minValue, int maxValue);                    // Scroll Bar control
(defcfun "GuiScrollBar" :int
  "Scroll Bar control"
  (bounds (:struct cl-raylib::%rectangle))
  (value :int)
  (min-value :int)
  (max-value :int))

;; RAYGUIAPI Vector2 GuiGrid(Rectangle bounds, float spacing, int subdivs);                                // Grid control
(defcfun "GuiGrid" (:struct cl-raylib::%vector2)
  "Grid control"
  (bounds (:struct cl-raylib::%rectangle))
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
(defcfun "GuiListViewEx" :int
  "List View with extended parameters"
  (bounds (:struct cl-raylib::%rectangle))
  (text (:pointer :string))
  (count :int)
  (focus (:pointer :int))
  (scroll-index (:pointer :int))
  (active :int))

;; RAYGUIAPI int GuiMessageBox(Rectangle bounds, const char *title, const char *message, const char *buttons);                 // Message Box control, displays a message
(defcfun "GuiMessageBox" :int
  "Message Box control, displays a message"
  (bounds (:struct cl-raylib::%rectangle))
  (title :string)
  (message :string)
  (buttons :string))

;; RAYGUIAPI int GuiTextInputBox(Rectangle bounds, const char *title, const char *message, const char *buttons, char *text);   // Text Input Box control, ask for text
(defcfun "GuiTextInputBox" :int
  (bounds (:struct cl-raylib::%rectangle))
  (title :string)
  (message :string)
  (buttons :string)
  (text :string)) ;; is there a difference between a 'const char *' and a 'char *'. Are they both ':string'?

;; RAYGUIAPI Color GuiColorPicker(Rectangle bounds, Color color);                                          // Color Picker control (multiple color controls)
(defcfun "GuiColorPicker" (:struct cl-raylib::%color)
  "Color Picker control, (multiple color controls)"
  (bounds (:struct cl-raylib::%rectangle))
  (color (:struct cl-raylib::%color)))

;; RAYGUIAPI Color GuiColorPanel(Rectangle bounds, Color color);                                           // Color Panel control
(defcfun "GuiColorPanel" (:struct cl-raylib::%color)
  "Color Panel Control"
  (bounds (:struct cl-raylib::%rectangle))
  (color (:struct cl-raylib::%color)))

;; RAYGUIAPI float GuiColorBarAlpha(Rectangle bounds, float alpha);                                        // Color Bar Alpha control
(defcfun "GuiColorBarAlpha" :float
  "Color Bar Alpha control"
  (bounds (:struct cl-raylib::%rectangle))
  (alpha :float))

;; RAYGUIAPI float GuiColorBarHue(Rectangle bounds, float value);                                          // Color Bar Hue control
(defcfun "GuiColorBarHue" :float
  "Color Bar Hue control"
  (bounds (:struct cl-raylib::%rectangle))
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
  (color (:struct cl-raylib::%color)))

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
(defcfun "GuiCheckIconPixel" :bool
  "Check icon pixel value"
  (icon-id :int)
  (x :int)
  (y :int))

;; #endif

;; #if defined(__cplusplus)
;; }            // Prevents name mangling of functions
;; #endif

;; #endif // RAYGUI_H
