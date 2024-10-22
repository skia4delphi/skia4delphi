<p><a href="https://www.skia4delphi.org"><img src="../Assets/Artwork/logo-gradient.svg" alt="Logo" height="300" width="360" /></a></p>

#  Animated images

## TSkAnimatedImage

**TSkAnimatedImage** is the control that can load and render animated images, including vector animations, in a very simple way. The supported formats are:

| Format           | Extensions     |
|------------------|----------------|
| Lottie file      | .json, .lottie |
| Telegram Sticker | .tgs           |
| Animated GIF     | .gif           |
| Animated WebP    | .webp          |

  

The example below demonstrates how to play Lottie files using **TSkAnimatedImage**:

```pascal
var
  LAnimatedImage: TSkAnimatedImage;
begin
  LAnimatedimage := TSkAnimatedImage.Create(Self);
  LAnimatedimage.LoadFromFile('Samples\Demo\Assets\rocket.json');
  LAnimatedimage.Parent := Self;
end;
```

The example above results in the output below:

![Rocket](../Assets/Documents/rocket.webp)

  

### TSkAnimatedImage.Source property

Contains the bytes of the animated image file. To load an animated image you can set the bytes to the `Source.Data` or just use the methods `LoadFromFile` or `LoadFromStream`.


### TSkAnimatedImage.Animation.Enabled property

Defines if the animation will run automatically when it becomes visible on screen.


### TSkAnimatedImage.Animation.Loop property

Defines if the animation will run in an infinite loop or if it will only run completly just 1 time.


### TSkAnimatedImage.WrapMode property

Specifies whether and how to resize and position the image draw inside the **TSkAnimatedImage** control. See the available options:


| TSkAnimatedImageWrapMode | Description                                                                                                                                                         |
|--------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Fit (Default)            | Stretches image into the destination rectangle, preserving aspect ratio                                                                                             |
| FitCrop                  | Stretches image into the destination rectangle, preserving aspect ratio, but cropping the excess                                                                    |
| Original                 | Display the image with its original dimensions                                                                                                                      |
| OriginalCenter           | Display the image with its original dimensions but centered                                                                                                         |
| Place                    | Places the image inside the destination rectangle. If the image is greater than the destination rectangle then the image is scaled down with aspect ratio preserved |
| Stretch                  | Stretch the image to fill the entire destination rectangle                                                                                                          |


### VCL

In **VCL** you cannot position a **TSkAnimatedImage** overlaying another animation.

  

## Lottie files

### What is?

Lottie files are vector animations in json format exported from Adobe After Effects. Like SVG, Lottie file is also a small file but capable of producing high quality animations. It was created by Airbnb and quickly became popular.

### Creating Lottie

You can create your own animation in Adobe After Effects, and using the plugin Bodymovin you will be able to export it in json format (Lottie file).

But if you have no design experience, there is no problem, there is a very large collection of free and paid animations (Lottie files) over the internet. The best website where you will find thousands of Lottie files is [lottiefiles.com](https://lottiefiles.com/). On this site, just choose the Lottie animation you want, click Download > Lottie JSON.

### Rendering Lottie

The code below shows how to load and render a Lottie file:

```pascal
var
  LAnimatedImage: TSkAnimatedImage;
begin
  LAnimatedImage := TSkAnimatedImage.Create(Self);
  LAnimatedImage.LoadFromFile('Samples\Demo\Assets\rocket.json');
  LAnimatedImage.Parent := Self;
end;
```

The example above results in the output below:

![Rocket](../Assets/Documents/rocket.webp)

  

## Telegram stickers

The Skia Lottie player *(Skottie)* does not support telegram stickers, but we added in **TSkAnimatedImage** support to telegram stickers *(tgs files)*. Tgs files are basically Lottie files compressed in GZIP format in max mode. That's why tgs files are usually 5x smaller than Lottie files.

So, to load you can simply use the code:

```pascal
var
  LAnimatedImage: TSkAnimatedImage;
begin
  LAnimatedImage := TSkAnimatedImage.Create(Self);
  LAnimatedImage.LoadFromFile('Samples\Demo\Assets\telegram-sticker.tgs');
  LAnimatedImage.Parent := Self;
end;
```

The example above results in the output below:

![Telegram Sticker](../Assets/Documents/telegram-sticker.webp)

**Tip:** You can download Telegram stickers easily using the Telegram app for Windows, and by right clicking on the sticker and clicking *Save As...* button.
