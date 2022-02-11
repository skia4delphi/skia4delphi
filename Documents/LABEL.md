<p><a href="https://www.skia4delphi.org"><img src="../Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

#  Label

## TSkLabel

**TSkLabel** is the control that implements the SkParagraph internally, having several more features than the TLabel, such as:

 - Font families; (font fallback list like in css)
 - Font weight;
 - Font slant;
 - Support for multiple styles in text;
 - Support for BiDi; (Right-to-Left)
 - Support justify horizontal alignment;
 - Support custom font; (without install the font)
 - Limit the maximum number of lines;
 - Auto size option; (width and height)
 - Advanced decorations; (like underline wavy, overline, dashed line, among others...)
   **and much more...**

![Label](https://user-images.githubusercontent.com/16469061/153615162-e2f51dd6-b22e-4f34-9493-244122faa5ae.png#gh-light-mode-only)
![Label](https://user-images.githubusercontent.com/16469061/153615217-53e851a3-c20d-4cb9-92fb-b9b18319c342.png#gh-dark-mode-only)

### Firemonkey styles

In Firemonkey, the TSkLabel supports FMX styles too. It is very recommended, because with that, you'll be able to automatically change the properties of your label, and create, for example, themes light and dark for your application, as in FMX's TLabel. To use styles in TSkLabel, follow the steps:

1. Open the Style Designer form (double click in the TStyleBook of your form)
2. Add a TLayout with the StyleName starting with "sklabelstyle", like "sklabelstyle" or "sklabelstyle_custom"
3. Drop inside this TLayout, a TSkStyleTextObject (in Skia palette)
4. Set the StyleName of the TSkStyleTextObject as "text"
5. Edit this last control (TSkStyleTextObject) any way you want, like set Color Red
6. Close the Style Designer form, will show a prompt asking to Apply, press Yes
7. Now, the style for TSkLabel that you created, will be available to select in TSkLabel.StyleLookup property

*Note: The style with the name "sklabelstyle" is always the default style of the TSkLabel.StyleLookup, even when it is empty.*

# Right-to-left

The right-to-left is made automatically after change the BiDi property of the form to RightToLeft.
