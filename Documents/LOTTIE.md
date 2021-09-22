<img src="/Assets/Artwork/LogoGradient.svg" width=360 height=200>

# Lottie files

## What is?

Lottie files are vector animations in json format exported from Adobe After Effects. Like SVG, Lottie file is also a small file but capable of producing high quality animations. It was created by Airbnb and quickly became popular.



## Creating Lottie files

You can create your own animation in Adobe After Effects, and using the plugin Bodymovin you will be able to export it in json format (lottie file).

But if you have no design practice, there is no problem, there is a very large collection of free and paid animations (Lottie files) over the internet. The official website where you will find thousands of Lottie files is [lottiefiles.com](https://lottiefiles.com/)



## Lottie in skia

The skia has made a player for lottie files, called Skottie (the class name here is TSkottie), but for faster programming you don't need to use it as we made a control just for rendering lottie animations, called TSkLottieAnimation.



## TSkLottieAnimation

The example below demonstrates how to play lottie files using our TSkLottieAnimation:

```pascal
var
  LLottie: TSkLottieAnimation;
begin
  LLottie := TSkLottieAnimation.Create(Self);
  LLottie.LoadFromFile('Assets\Samples\rocket.json');
  LLottie.Parent := Self;
end;
```

The **Assets/Samples/rocket.json** file animation results in the output below:

![skottie](../Assets/Documents/rocket.gif)


#### property Source

String property of lottie file content, the json text. You can also load a lottie file with the methods LoadFromFile or LoadFromStream.


#### property Loop

Defines if the animation will run in an infinite loop or if it will only run 1 time.


#### Play or Stop

To simplify control even further, play and stop operations are done automatically, no method needs to be called. Every time the control becomes visible, animation is started, and every time the control is invisible or disabled, the animation is stopped.


#### VCL

In Vcl you should use the TSkLottieAnimation inside a form or a control with DoubleBuffered.



## Telegram stickers

The skia lottie player (skottie) does not support telegram stickers, but we improved the TSkLottieAnimation to support telegram stickers (tgs files). Tgs files are basically lottie files compressed in GZIP format in max mode. That's why tgs files are usually 5x smaller than lottie files.

So, to load you can simple use the code:

```pascal
var
  LLottie: TSkLottieAnimation;
begin
  LLottie := TSkLottieAnimation.Create(Self);
  LLottie.LoadFromFile('Assets\Samples\telegram_sticker.tgs');
  LLottie.Parent := Self;
end;
```

The **Assets/Samples/telegram_sticker.tgs** file animation results in the output below:

![skottie](../Assets/Documents/telegram_sticker.gif)
