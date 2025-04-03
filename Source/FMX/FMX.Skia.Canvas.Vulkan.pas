{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2023-2025 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Skia.Canvas.Vulkan;

interface

{$SCOPEDENUMS ON}
{$HPPEMIT NOUSINGNAMESPACE}

{$IF DEFINED(MSWINDOWS) or DEFINED(ANDROID)}
  {$DEFINE SKIA_VULKAN}
{$ENDIF}

{$IFDEF SKIA_VULKAN}

uses
  { Delphi }
  {$IF DEFINED(ANDROID)}
  Androidapi.Vulkan,
  {$ELSEIF DEFINED(MSWINDOWS)}
  Winapi.Vulkan,
  {$ENDIF}
  System.SysUtils,
  System.Vulkan,

  { Skia }
  System.Skia,
  FMX.Skia.Canvas;

type
  { TVkInterface }

  TVkInterface = record
  private
    FAcquireNextImageKHR: PFN_vkAcquireNextImageKHR;
    FCreateDevice: PFN_vkCreateDevice;
    FCreateInstance: PFN_vkCreateInstance;
    FCreateSemaphore: PFN_vkCreateSemaphore;
    FCreateSwapchainKHR: PFN_vkCreateSwapchainKHR;
    FDestroyDevice: PFN_vkDestroyDevice;
    FDestroyInstance: PFN_vkDestroyInstance;
    FDestroySemaphore: PFN_vkDestroySemaphore;
    FDestroySurfaceKHR: PFN_vkDestroySurfaceKHR;
    FDestroySwapchainKHR: PFN_vkDestroySwapchainKHR;
    FDeviceWaitIdle: PFN_vkDeviceWaitIdle;
    FEnumerateDeviceExtensionProperties: PFN_vkEnumerateDeviceExtensionProperties;
    FEnumerateInstanceExtensionProperties: PFN_vkEnumerateInstanceExtensionProperties;
    FEnumerateInstanceVersion: PFN_vkEnumerateInstanceVersion;
    FEnumeratePhysicalDevices: PFN_vkEnumeratePhysicalDevices;
    FGetDeviceProcAddr: PFN_vkGetDeviceProcAddr;
    FGetDeviceQueue: PFN_vkGetDeviceQueue;
    FGetInstanceProcAddr: PFN_vkGetInstanceProcAddr;
    FGetPhysicalDeviceFeatures: PFN_vkGetPhysicalDeviceFeatures;
    FGetPhysicalDeviceFeatures2: PFN_vkGetPhysicalDeviceFeatures2;
    FGetPhysicalDeviceProperties: PFN_vkGetPhysicalDeviceProperties;
    FGetPhysicalDeviceQueueFamilyProperties: PFN_vkGetPhysicalDeviceQueueFamilyProperties;
    FGetPhysicalDeviceSurfaceCapabilitiesKHR: PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR;
    FGetPhysicalDeviceSurfaceFormatsKHR: PFN_vkGetPhysicalDeviceSurfaceFormatsKHR;
    FGetPhysicalDeviceSurfacePresentModesKHR: PFN_vkGetPhysicalDeviceSurfacePresentModesKHR;
    FGetSwapchainImagesKHR: PFN_vkGetSwapchainImagesKHR;
    FQueuePresentKHR: PFN_vkQueuePresentKHR;
    FQueueWaitIdle: PFN_vkQueueWaitIdle;
    {$IF DEFINED(ANDROID)}
    FCreateAndroidSurfaceKHR: PFN_vkCreateAndroidSurfaceKHR;
    {$ELSEIF DEFINED(MSWINDOWS)}
    FCreateWin32SurfaceKHR: PFN_vkCreateWin32SurfaceKHR;
    FGetPhysicalDeviceWin32PresentationSupportKHR: PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR;
    {$ENDIF}
    procedure AcquireDestroyInstanceProc(const AInstance: VkInstance); inline;
    procedure AcquireDeviceProcs(const ADevice: VkDevice);
    procedure AcquireExtensionProcs(const AInstance: VkInstance; const APhysicalDeviceApiVersion: Cardinal; const AGrVkExtensions: IGrVkExtensions);
    procedure AcquireInstanceProcs(const AInstance: VkInstance);
    procedure GetDeviceProc(const ADevice: VkDevice; const AName: MarshaledAString; out APFN_vkFunction);
    procedure GetInstanceProc(const AInstance: VkInstance; const AName: MarshaledAString; out APFN_vkFunction; const ARaiseIfNotExist: Boolean = True);
    procedure Initialize(const ALibraryHandle: HMODULE);
  public
    { Global Procs }
    property CreateInstance: PFN_vkCreateInstance read FCreateInstance;
    property EnumerateInstanceExtensionProperties: PFN_vkEnumerateInstanceExtensionProperties read FEnumerateInstanceExtensionProperties;
    property EnumerateInstanceVersion: PFN_vkEnumerateInstanceVersion read FEnumerateInstanceVersion;
    property GetInstanceProcAddr: PFN_vkGetInstanceProcAddr read FGetInstanceProcAddr;

    { Instance Procs }
    property CreateDevice: PFN_vkCreateDevice read FCreateDevice;
    property DestroyDevice: PFN_vkDestroyDevice read FDestroyDevice;
    property DestroyInstance: PFN_vkDestroyInstance read FDestroyInstance;
    property EnumerateDeviceExtensionProperties: PFN_vkEnumerateDeviceExtensionProperties read FEnumerateDeviceExtensionProperties;
    property EnumeratePhysicalDevices: PFN_vkEnumeratePhysicalDevices read FEnumeratePhysicalDevices;
    property GetDeviceProcAddr: PFN_vkGetDeviceProcAddr read FGetDeviceProcAddr;
    property GetPhysicalDeviceFeatures: PFN_vkGetPhysicalDeviceFeatures read FGetPhysicalDeviceFeatures;
    property GetPhysicalDeviceProperties: PFN_vkGetPhysicalDeviceProperties read FGetPhysicalDeviceProperties;
    property GetPhysicalDeviceQueueFamilyProperties: PFN_vkGetPhysicalDeviceQueueFamilyProperties read FGetPhysicalDeviceQueueFamilyProperties;
    property DestroySurfaceKHR: PFN_vkDestroySurfaceKHR read FDestroySurfaceKHR;
    property GetPhysicalDeviceSurfaceCapabilitiesKHR: PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR read FGetPhysicalDeviceSurfaceCapabilitiesKHR;
    property GetPhysicalDeviceSurfaceFormatsKHR: PFN_vkGetPhysicalDeviceSurfaceFormatsKHR read FGetPhysicalDeviceSurfaceFormatsKHR;
    property GetPhysicalDeviceSurfacePresentModesKHR: PFN_vkGetPhysicalDeviceSurfacePresentModesKHR read FGetPhysicalDeviceSurfacePresentModesKHR;
    {$IF DEFINED(ANDROID)}
    property CreateAndroidSurfaceKHR: PFN_vkCreateAndroidSurfaceKHR read FCreateAndroidSurfaceKHR;
    {$ELSEIF DEFINED(MSWINDOWS)}
    property CreateWin32SurfaceKHR: PFN_vkCreateWin32SurfaceKHR read FCreateWin32SurfaceKHR;
    property GetPhysicalDeviceWin32PresentationSupportKHR: PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR read FGetPhysicalDeviceWin32PresentationSupportKHR;
    {$ENDIF}

    { Instance Extension Procs }
    property GetPhysicalDeviceFeatures2: PFN_vkGetPhysicalDeviceFeatures2 read FGetPhysicalDeviceFeatures2;

    { Device Procs }
    property CreateSemaphore: PFN_vkCreateSemaphore read FCreateSemaphore;
    property DestroySemaphore: PFN_vkDestroySemaphore read FDestroySemaphore;
    property DeviceWaitIdle: PFN_vkDeviceWaitIdle read FDeviceWaitIdle;
    property GetDeviceQueue: PFN_vkGetDeviceQueue read FGetDeviceQueue;
    property QueueWaitIdle: PFN_vkQueueWaitIdle read FQueueWaitIdle;
    property AcquireNextImageKHR: PFN_vkAcquireNextImageKHR read FAcquireNextImageKHR;
    property CreateSwapchainKHR: PFN_vkCreateSwapchainKHR read FCreateSwapchainKHR;
    property DestroySwapchainKHR: PFN_vkDestroySwapchainKHR read FDestroySwapchainKHR;
    property GetSwapchainImagesKHR: PFN_vkGetSwapchainImagesKHR read FGetSwapchainImagesKHR;
    property QueuePresentKHR: PFN_vkQueuePresentKHR read FQueuePresentKHR;
  end;

  { TVkSharedContextCustom }

  TVkSharedContextCustom = class abstract(TGrSharedContext)
  protected
    FDevice: VkDevice;
    FGraphicsQueue: VkQueue;
    FGraphicsQueueIndex: Cardinal;
    FInstance: VkInstance;
    FPhysicalDevice: VkPhysicalDevice;
    FPresentQueue: VkQueue;
    FPresentQueueIndex: Cardinal;
    FVkInterface: TVkInterface;
  public
    property Device: VkDevice read FDevice;
    property GraphicsQueue: VkQueue read FGraphicsQueue;
    property GraphicsQueueIndex: Cardinal read FGraphicsQueueIndex;
    property Instance: VkInstance read FInstance;
    property PhysicalDevice: VkPhysicalDevice read FPhysicalDevice;
    property PresentQueue: VkQueue read FPresentQueue;
    property PresentQueueIndex: Cardinal read FPresentQueueIndex;
    property VkInterface: TVkInterface read FVkInterface;
  end;

var
  GlobalUseSkiaVulkanFifoKHR: Boolean;

implementation

uses
  { Delphi }
  FMX.Graphics,
  FMX.Types,
  {$IF DEFINED(ANDROID)}
  FMX.Platform.UI.Android,
  FMX.Presentation.Android.Style,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.NativeWindow,
  Androidapi.NativeWindowJni,
  {$ELSEIF DEFINED(MSWINDOWS)}
  FMX.Platform.Win,
  Winapi.Windows,
  {$ENDIF}
  System.AnsiStrings,
  System.Math;

type
  EVkError = class(EGrCanvas);

  { TVkCanvas }

  TVkCanvas = class(TGrCanvas)
  private type
    TBackBufferInfo = record
      Index: Cardinal;
      Semaphore: VkSemaphore;
    end;

  private const
    SemaphoreCreateInfo: VkSemaphoreCreateInfo = (sType: VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO; pNext: nil; flags: 0);
  private
    {$IFDEF ANDROID}
    FANativeWindow: PANativeWindow;
    {$ENDIF}
    FBackBuffers: TArray<TBackBufferInfo>;
    FBackBufferSurfaces: TArray<ISkSurface>;
    FCurrentBackBufferIndex: Integer;
    FImageLayouts: TArray<VkImageLayout>;
    FImages: TArray<VkImage>;
    FSwapchain: VkSwapchainKHR;
    FVkSurface: VkSurfaceKHR;
    function CreateSurface: Boolean;
    function CreateSwapchain: Boolean;
    procedure DestroySurface;
    procedure DestroySwapchain;
  protected
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    function CreateSharedContext: IGrSharedContext; override;
    function GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface; override;
    procedure Resized; override;
    procedure SwapBuffers(const AContextHandle: THandle); override;
  public
    destructor Destroy; override;
    class function IsSupported: Boolean;
  end;

  { TVkSharedContext }

  TVkSharedContext = class(TVkSharedContextCustom)
  private
    FFeatures: VkPhysicalDeviceFeatures2;
    FLibraryHandle: HMODULE;
    procedure DestroyFeatures;
  protected
    procedure DestroyContext; override;
    function GetTextureColorType: TSkColorType; override;
    function GetTextureOrigin: TGrSurfaceOrigin; override;
  public
    constructor Create;
  end;

procedure VkCheckSuccess(const AResult: VkResult);

  function ErrorToStr(const AResult: VkResult): string; inline;
  begin
    case AResult of
      VK_NOT_READY                     : Result := 'VK_NOT_READY';
      VK_TIMEOUT                       : Result := 'VK_TIMEOUT';
      VK_EVENT_SET                     : Result := 'VK_EVENT_SET';
      VK_EVENT_RESET                   : Result := 'VK_EVENT_RESET';
      VK_INCOMPLETE                    : Result := 'VK_INCOMPLETE';
      VK_ERROR_OUT_OF_HOST_MEMORY      : Result := 'VK_ERROR_OUT_OF_HOST_MEMORY';
      VK_ERROR_OUT_OF_DEVICE_MEMORY    : Result := 'VK_ERROR_OUT_OF_DEVICE_MEMORY';
      VK_ERROR_INITIALIZATION_FAILED   : Result := 'VK_ERROR_INITIALIZATION_FAILED';
      VK_ERROR_DEVICE_LOST             : Result := 'VK_ERROR_DEVICE_LOST';
      VK_ERROR_MEMORY_MAP_FAILED       : Result := 'VK_ERROR_MEMORY_MAP_FAILED';
      VK_ERROR_LAYER_NOT_PRESENT       : Result := 'VK_ERROR_LAYER_NOT_PRESENT';
      VK_ERROR_EXTENSION_NOT_PRESENT   : Result := 'VK_ERROR_EXTENSION_NOT_PRESENT';
      VK_ERROR_FEATURE_NOT_PRESENT     : Result := 'VK_ERROR_FEATURE_NOT_PRESENT';
      VK_ERROR_INCOMPATIBLE_DRIVER     : Result := 'VK_ERROR_INCOMPATIBLE_DRIVER';
      VK_ERROR_TOO_MANY_OBJECTS        : Result := 'VK_ERROR_TOO_MANY_OBJECTS';
      VK_ERROR_FORMAT_NOT_SUPPORTED    : Result := 'VK_ERROR_FORMAT_NOT_SUPPORTED';
      VK_ERROR_FRAGMENTED_POOL         : Result := 'VK_ERROR_FRAGMENTED_POOL';
      VK_ERROR_UNKNOWN                 : Result := 'VK_ERROR_UNKNOWN';
      VK_ERROR_OUT_OF_POOL_MEMORY      : Result := 'VK_ERROR_OUT_OF_POOL_MEMORY';
      VK_ERROR_INVALID_EXTERNAL_HANDLE : Result := 'VK_ERROR_INVALID_EXTERNAL_HANDLE';
      VK_ERROR_SURFACE_LOST_KHR        : Result := 'VK_ERROR_SURFACE_LOST_KHR';
      VK_ERROR_NATIVE_WINDOW_IN_USE_KHR: Result := 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR';
      VK_SUBOPTIMAL_KHR                : Result := 'VK_SUBOPTIMAL_KHR';
      VK_ERROR_OUT_OF_DATE_KHR         : Result := 'VK_ERROR_OUT_OF_DATE_KHR';
    else
      Result := AResult.ToString;
    end;
  end;

begin
  if AResult <> VK_SUCCESS then
    raise EVkError.CreateFmt('Vulkan API call failed with error: %s', [ErrorToStr(AResult)]) at ReturnAddress;
end;

function VkLoadLibrary: HMODULE; inline;
begin
  {$IF DEFINED(ANDROID)}
  Result := SafeLoadLibrary('libvulkan.so');
  // In an attempt to load the library, Vulkan Info searches for it in two
  // distinct locations, and as a result, we replicate this behavior.
  //
  // https://github.com/KhronosGroup/Vulkan-Tools/blob/078d44e4664b7efa0b6c96ebced1995c4425d57a/vulkaninfo/vulkaninfo.h#L249
  if Result = 0 then
    Result := SafeLoadLibrary('libvulkan.so.1');
  //{$ELSEIF DEFINED(LINUX)}
  //Result := SafeLoadLibrary('libvulkan.so.1');
  //{$ELSEIF DEFINED(MACOS)}
  //Result := SafeLoadLibrary('libvk_swiftshader.dylib');
  {$ELSEIF DEFINED(MSWINDOWS)}
  Result := SafeLoadLibrary('vulkan-1.dll');
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

{ TVkCanvas }

constructor TVkCanvas.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  FGrDirectContext := TGrSharedContext(SharedContext).GrDirectContext;
end;

function TVkCanvas.CreateSharedContext: IGrSharedContext;
begin
  Result := TVkSharedContext.Create;
end;

function TVkCanvas.CreateSurface: Boolean;
{$IF DEFINED(ANDROID)}
var
  LJSurface: JSurface;
  LSurface: Pointer;
  LSurfaceCreateInfo: VkAndroidSurfaceCreateInfoKHR;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  LSurfaceCreateInfo: VkWin32SurfaceCreateInfoKHR;
{$ENDIF}
begin
  {$IF DEFINED(ANDROID)}
  Result := False;
  if Parent is TAndroidWindowHandle then
  begin
    if TAndroidWindowHandle(Parent).Holder = nil then
      Exit;
    LSurface := (TAndroidWindowHandle(Parent).Holder.getSurface as ILocalObject).GetObjectID;
  end
  else if Parent is TAndroidHandle then
  begin
    if TAndroidHandle(Parent).Surface = nil then
      Exit;
    LJSurface := TJSurface.JavaClass.init(TAndroidHandle(Parent).Surface);
    LSurface  := TJNIResolver.JavaInstanceToID(LJSurface);
  end
  else
    Exit;
  FANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, LSurface);
  if FANativeWindow = nil then
    Exit;
  try
    LSurfaceCreateInfo.sType  := VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
    LSurfaceCreateInfo.pNext  := nil;
    LSurfaceCreateInfo.flags  := 0;
    LSurfaceCreateInfo.window := FANativeWindow;
    Result := TVkSharedContext(SharedContext).VkInterface.CreateAndroidSurfaceKHR(TVkSharedContext(SharedContext).Instance, @LSurfaceCreateInfo, nil, @FVkSurface) = VK_SUCCESS;
  finally
    if not Result then
      ANativeWindow_release(FANativeWindow);
  end;
  {$ELSEIF DEFINED(MSWINDOWS)}
  LSurfaceCreateInfo.sType     := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
  LSurfaceCreateInfo.pNext     := nil;
  LSurfaceCreateInfo.flags     := 0;
  LSurfaceCreateInfo.hinstance := HInstance;
  LSurfaceCreateInfo.hwnd      := WindowHandleToPlatform(Parent).Wnd;
  Result := TVkSharedContext(SharedContext).VkInterface.CreateWin32SurfaceKHR(TVkSharedContext(SharedContext).Instance, @LSurfaceCreateInfo, nil, @FVkSurface) = VK_SUCCESS;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TVkCanvas.CreateSwapchain: Boolean;

  function CreateBuffers(const ASwapchain: VkSwapchainKHR;
    const AFormat: VkFormat; const AColorType: TSkColorType;
    const AExtent: VkExtent2D; const AUsageFlags: VkImageUsageFlags;
    const ASharingMode: VkSharingMode): Boolean;
  var
    I: Integer;
    J: Integer;
    LBackBuffers: TArray<TBackBufferInfo>;
    LCount: Cardinal;
    LGrBackendRenderTarget: IGrBackendRenderTarget;
    LGrBackendTexture: IGrBackendTexture;
    LGrVkImageInfo: TGrVkImageInfo;
    LImageLayouts: TArray<VkImageLayout>;
    LImages: TArray<VkImage>;
    LSurfaces: TArray<ISkSurface>;
  begin
    if TVkSharedContext(SharedContext).VkInterface.GetSwapchainImagesKHR(TVkSharedContext(SharedContext).Device, ASwapchain, @LCount, nil) <> VK_SUCCESS then
      Exit(False);
    SetLength(LImages, LCount);
    if TVkSharedContext(SharedContext).VkInterface.GetSwapchainImagesKHR(TVkSharedContext(SharedContext).Device, ASwapchain, @LCount, Pointer(LImages)) <> VK_SUCCESS then
      Exit(False);
    SetLength(LImageLayouts, LCount);
    SetLength(LSurfaces, LCount);
    for I := 0 to Length(LImages) - 1 do
    begin
      LImageLayouts[I] := VK_IMAGE_LAYOUT_UNDEFINED;
      LGrVkImageInfo.Image := LImages[I];
      LGrVkImageInfo.Alloc.DeviceMemory := VK_NULL_HANDLE;
      LGrVkImageInfo.Alloc.Offset       := 0;
      LGrVkImageInfo.Alloc.Size         := 0;
      LGrVkImageInfo.Alloc.Flags        := [];
      LGrVkImageInfo.Alloc.Memory       := 0;
      LGrVkImageInfo.ImageTiling        := VK_IMAGE_TILING_OPTIMAL;
      LGrVkImageInfo.ImageLayout        := VK_IMAGE_LAYOUT_UNDEFINED;
      LGrVkImageInfo.Format             := AFormat;
      LGrVkImageInfo.ImageUsageFlags    := AUsageFlags;
      LGrVkImageInfo.SampleCount        := 1;
      LGrVkImageInfo.LevelCount         := 1;
      LGrVkImageInfo.CurrentQueueFamily := TVkSharedContext(SharedContext).PresentQueueIndex;
      LGrVkImageInfo.ProtectedImage     := False;
      LGrVkImageInfo.YcbcrConversionInfo.Format                      := VK_FORMAT_UNDEFINED;
      LGrVkImageInfo.YcbcrConversionInfo.FExternalFormat             := 0;
      LGrVkImageInfo.YcbcrConversionInfo.YcbcrModel                  := VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY;
      LGrVkImageInfo.YcbcrConversionInfo.YcbcrRange                  := VK_SAMPLER_YCBCR_RANGE_ITU_FULL;
      LGrVkImageInfo.YcbcrConversionInfo.XChromaOffset               := VK_CHROMA_LOCATION_COSITED_EVEN;
      LGrVkImageInfo.YcbcrConversionInfo.YChromaOffset               := VK_CHROMA_LOCATION_COSITED_EVEN;
      LGrVkImageInfo.YcbcrConversionInfo.ChromaFilter                := VK_FILTER_NEAREST;
      LGrVkImageInfo.YcbcrConversionInfo.ForceExplicitReconstruction := VK_FALSE;
      LGrVkImageInfo.YcbcrConversionInfo.FormatFeatures              := 0;
      LGrVkImageInfo.SharingMode := ASharingMode;
      if (AUsageFlags and VK_IMAGE_USAGE_SAMPLED_BIT) <> 0 then
      begin
        LGrBackendTexture := TGrBackendTexture.CreateVulkan(AExtent.width, AExtent.height, LGrVkImageInfo);
        LSurfaces[I]      := TSkSurface.MakeFromTexture(FGrDirectContext, LGrBackendTexture, TGrSurfaceOrigin.TopLeft, Min(CanvasQualitySampleCount[Quality], FGrDirectContext.GetMaxSurfaceSampleCountForColorType(AColorType)), AColorType);
      end
      else
      begin
        LGrBackendRenderTarget := TGrBackendRenderTarget.CreateVulkan(AExtent.width, AExtent.height, LGrVkImageInfo);
        LSurfaces[I]           := TSkSurface.MakeFromRenderTarget(FGrDirectContext, LGrBackendRenderTarget, TGrSurfaceOrigin.TopLeft, AColorType);
      end;
      if LSurfaces[I] = nil then
        Exit(False);
    end;
    SetLength(LBackBuffers, LCount + 1);
    for I := 0 to LCount do
    begin
      LBackBuffers[I].Index := Cardinal(-1);
      if TVkSharedContext(SharedContext).VkInterface.CreateSemaphore(TVkSharedContext(SharedContext).Device, @SemaphoreCreateInfo, nil, @LBackBuffers[I].Semaphore) <> VK_SUCCESS then
      begin
        for J := 0 to I - 1 do
          TVkSharedContext(SharedContext).VkInterface.DestroySemaphore(TVkSharedContext(SharedContext).Device, LBackBuffers[J].Semaphore, nil);
        Exit(False);
      end;
    end;
    FBackBuffers            := LBackBuffers;
    FBackBufferSurfaces     := LSurfaces;
    FCurrentBackBufferIndex := LCount;
    FImageLayouts           := LImageLayouts;
    FImages                 := LImages;
    Result                  := True;
  end;

var
  I: Integer;
  LCapabilities: VkSurfaceCapabilitiesKHR;
  LColorType: TSkColorType;
  LCompositeAlpha: VkCompositeAlphaFlagBitsKHR;
  LCount: Cardinal;
  LExtent: VkExtent2D;
  LFormat: VkSurfaceFormatKHR;
  LFormats: TArray<VkSurfaceFormatKHR>;
  LImageCount: Cardinal;
  LPresentMode: VkPresentModeKHR;
  LPresentModes: TArray<VkPresentModeKHR>;
  LQueueFamilyIndices: array[0..1] of Cardinal;
  LSwapchain: VkSwapchainKHR;
  LSwapchainCreateInfo: VkSwapchainCreateInfoKHR;
  LUsageFlags: VkImageUsageFlags;
begin
  Result := False;
  if TVkSharedContext(SharedContext).VkInterface.GetPhysicalDeviceSurfaceCapabilitiesKHR(TVkSharedContext(SharedContext).PhysicalDevice, FVkSurface, @LCapabilities) <> VK_SUCCESS then
    Exit;
  if TVkSharedContext(SharedContext).VkInterface.GetPhysicalDeviceSurfaceFormatsKHR(TVkSharedContext(SharedContext).PhysicalDevice, FVkSurface, @LCount, nil) <> VK_SUCCESS then
    Exit;
  SetLength(LFormats, LCount);
  if TVkSharedContext(SharedContext).VkInterface.GetPhysicalDeviceSurfaceFormatsKHR(TVkSharedContext(SharedContext).PhysicalDevice, FVkSurface, @LCount, Pointer(LFormats)) <> VK_SUCCESS then
    Exit;
  if TVkSharedContext(SharedContext).VkInterface.GetPhysicalDeviceSurfacePresentModesKHR(TVkSharedContext(SharedContext).PhysicalDevice, FVkSurface, @LCount, nil) <> VK_SUCCESS then
    Exit;
  SetLength(LPresentModes, LCount);
  if TVkSharedContext(SharedContext).VkInterface.GetPhysicalDeviceSurfacePresentModesKHR(TVkSharedContext(SharedContext).PhysicalDevice, FVkSurface, @LCount, Pointer(LPresentModes)) <> VK_SUCCESS then
    Exit;
  LExtent := LCapabilities.currentExtent;
  if LExtent.width = Cardinal(-1) then
  begin
    LExtent.width  := Round(Width  * Scale);
    LExtent.height := Round(Height * Scale);
  end;
  if LExtent.width < LCapabilities.minImageExtent.width then
    LExtent.width := LCapabilities.minImageExtent.width
  else if LExtent.width > LCapabilities.maxImageExtent.width then
    LExtent.width := LCapabilities.maxImageExtent.width;
  if LExtent.height < LCapabilities.minImageExtent.height then
    LExtent.height := LCapabilities.minImageExtent.height
  else if LExtent.height > LCapabilities.maxImageExtent.height then
    LExtent.height := LCapabilities.maxImageExtent.height;
  LImageCount := LCapabilities.minImageCount + 2;
  if (LCapabilities.maxImageCount > 0) and (LImageCount > LCapabilities.maxImageCount) then
    LImageCount := LCapabilities.maxImageCount;
  LUsageFlags := VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT or VK_IMAGE_USAGE_TRANSFER_SRC_BIT or VK_IMAGE_USAGE_TRANSFER_DST_BIT;
  if (LCapabilities.supportedUsageFlags and VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) <> 0 then
    LUsageFlags := LUsageFlags or VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT;
  if (LCapabilities.supportedUsageFlags and VK_IMAGE_USAGE_SAMPLED_BIT) <> 0 then
    LUsageFlags := LUsageFlags or VK_IMAGE_USAGE_SAMPLED_BIT;
  if (LCapabilities.supportedCompositeAlpha and VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR) <> 0 then
    LCompositeAlpha := VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
  else
    LCompositeAlpha := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
  LColorType := TSkColorType.Unknown;
  for I := 0 to Length(LFormats) - 1 do
  begin
    case LFormats[I].format of
      VK_FORMAT_R8G8B8A8_UNORM,
      VK_FORMAT_R8G8B8A8_SRGB,
      VK_FORMAT_B8G8R8A8_UNORM:
        begin
          LFormat := LFormats[I];
          if LFormats[I].format = VK_FORMAT_B8G8R8A8_UNORM then
            LColorType := TSkColorType.BGRA8888
          else
            LColorType := TSkColorType.RGBA8888;
          Break;
        end;
    end;
  end;
  if LColorType = TSkColorType.Unknown then
    Exit;
  LPresentMode := VK_PRESENT_MODE_FIFO_KHR;
  if not GlobalUseSkiaVulkanFifoKHR then
    for I := 0 to Length(LPresentModes) - 1 do
    begin
      if LPresentModes[I] = VK_PRESENT_MODE_MAILBOX_KHR then
        LPresentMode := VK_PRESENT_MODE_MAILBOX_KHR;
      if LPresentModes[I] = VK_PRESENT_MODE_IMMEDIATE_KHR then
      begin
        LPresentMode := VK_PRESENT_MODE_IMMEDIATE_KHR;
        Break;
      end;
    end;
  LSwapchainCreateInfo.sType            := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
  LSwapchainCreateInfo.pNext            := nil;
  LSwapchainCreateInfo.flags            := 0;
  LSwapchainCreateInfo.surface          := FVkSurface;
  LSwapchainCreateInfo.minImageCount    := LImageCount;
  LSwapchainCreateInfo.imageFormat      := LFormat.format;
  LSwapchainCreateInfo.imageColorSpace  := LFormat.colorSpace;
  LSwapchainCreateInfo.imageExtent      := LExtent;
  LSwapchainCreateInfo.imageArrayLayers := 1;
  LSwapchainCreateInfo.imageUsage       := LUsageFlags;
  if TVkSharedContext(SharedContext).GraphicsQueueIndex <> TVkSharedContext(SharedContext).PresentQueueIndex then
  begin
    LQueueFamilyIndices[0] := TVkSharedContext(SharedContext).GraphicsQueueIndex;
    LQueueFamilyIndices[1] := TVkSharedContext(SharedContext).PresentQueueIndex;
    LSwapchainCreateInfo.imageSharingMode      := VK_SHARING_MODE_CONCURRENT;
    LSwapchainCreateInfo.queueFamilyIndexCount := 2;
    LSwapchainCreateInfo.pQueueFamilyIndices   := @LQueueFamilyIndices;
  end
  else
  begin
    LSwapchainCreateInfo.imageSharingMode      := VK_SHARING_MODE_EXCLUSIVE;
    LSwapchainCreateInfo.queueFamilyIndexCount := 0;
    LSwapchainCreateInfo.pQueueFamilyIndices   := nil;
  end;
  LSwapchainCreateInfo.preTransform   := VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
  LSwapchainCreateInfo.compositeAlpha := LCompositeAlpha;
  LSwapchainCreateInfo.presentMode    := LPresentMode;
  LSwapchainCreateInfo.clipped        := VK_TRUE;
  LSwapchainCreateInfo.oldSwapchain   := FSwapchain;
  if TVkSharedContext(SharedContext).VkInterface.CreateSwapchainKHR(TVkSharedContext(SharedContext).Device, @LSwapchainCreateInfo, nil, @LSwapchain) <> VK_SUCCESS then
    Exit;
  try
    if LSwapchainCreateInfo.oldSwapchain <> VK_NULL_HANDLE then
      DestroySwapchain;
    if not CreateBuffers(LSwapchain, LSwapchainCreateInfo.imageFormat, LColorType, LSwapchainCreateInfo.imageExtent, LSwapchainCreateInfo.imageUsage, LSwapchainCreateInfo.imageSharingMode) then
      Exit;
    FSwapchain := LSwapchain;
    Result     := FSwapchain <> VK_NULL_HANDLE;
  finally
    if not Result then
      TVkSharedContext(SharedContext).VkInterface.DestroySwapchainKHR(TVkSharedContext(SharedContext).Device, LSwapchain, nil);
  end;
end;

destructor TVkCanvas.Destroy;
begin
  if Parent <> nil then
  begin
    TVkSharedContext(SharedContext).VkInterface.QueueWaitIdle(TVkSharedContext(SharedContext).PresentQueue);
    DestroySwapchain;
    DestroySurface;
    FGrDirectContext := nil;
  end;
  inherited;
end;

procedure TVkCanvas.DestroySurface;
begin
  if FVkSurface <> VK_NULL_HANDLE then
  begin
    TVkSharedContext(SharedContext).VkInterface.DestroySurfaceKHR(TVkSharedContext(SharedContext).Instance, FVkSurface, nil);
    FVkSurface := VK_NULL_HANDLE;
    {$IFDEF ANDROID}
    ANativeWindow_release(FANativeWindow);
    {$ENDIF}
  end;
end;

procedure TVkCanvas.DestroySwapchain;

  procedure DestroyBuffers;
  var
    I: Integer;
  begin
    FBackBufferSurfaces := nil;
    FImages             := nil;
    FImageLayouts       := nil;
    for I := 0 to Length(FBackBuffers) - 1 do
      TVkSharedContext(SharedContext).VkInterface.DestroySemaphore(TVkSharedContext(SharedContext).Device, FBackBuffers[I].Semaphore, nil);
    FBackBuffers := nil;
  end;

begin
  if FSwapchain <> VK_NULL_HANDLE then
  begin
    TVkSharedContext(SharedContext).VkInterface.DeviceWaitIdle(TVkSharedContext(SharedContext).Device);
    DestroyBuffers;
    TVkSharedContext(SharedContext).VkInterface.DestroySwapchainKHR(TVkSharedContext(SharedContext).Device, FSwapchain, nil);
    FSwapchain := VK_NULL_HANDLE;
  end;
end;

function TVkCanvas.GetSurfaceFromWindow(
  const AContextHandle: THandle): TSkSurface;
var
  LGrBackendSemaphore: IGrBackendSemaphore;
  LResult: VkResult;
  LSemaphore: VkSemaphore;
begin
  Result := nil;
  SharedContext.BeginContext;
  try
    if ((FVkSurface = VK_NULL_HANDLE) and (not CreateSurface)) or ((FSwapchain = VK_NULL_HANDLE) and (not CreateSwapchain)) then
      Exit;
    if TVkSharedContext(SharedContext).VkInterface.CreateSemaphore(TVkSharedContext(SharedContext).Device, @SemaphoreCreateInfo, nil, @LSemaphore) <> VK_SUCCESS then
      Exit;
    Inc(FCurrentBackBufferIndex);
    if FCurrentBackBufferIndex > Length(FImages) then
      FCurrentBackBufferIndex := 0;
    LResult := TVkSharedContext(SharedContext).VkInterface.AcquireNextImageKHR(TVkSharedContext(SharedContext).Device, FSwapchain, High(UInt64), LSemaphore, VK_NULL_HANDLE, @FBackBuffers[FCurrentBackBufferIndex].Index);
    if (LResult = VK_ERROR_SURFACE_LOST_KHR) or (LResult = VK_ERROR_OUT_OF_DATE_KHR)  then
    begin
      TVkSharedContext(SharedContext).VkInterface.DestroySemaphore(TVkSharedContext(SharedContext).Device, LSemaphore, nil);
      if LResult = VK_ERROR_SURFACE_LOST_KHR then
      begin
        DestroySwapchain;
        DestroySurface;
        if not CreateSurface then
          Exit;
      end;
      if not CreateSwapchain then
        Exit;
      if TVkSharedContext(SharedContext).VkInterface.CreateSemaphore(TVkSharedContext(SharedContext).Device, @SemaphoreCreateInfo, nil, @LSemaphore) <> VK_SUCCESS then
        Exit;
      Inc(FCurrentBackBufferIndex);
      if FCurrentBackBufferIndex > Length(FImages) then
        FCurrentBackBufferIndex := 0;
      if TVkSharedContext(SharedContext).VkInterface.AcquireNextImageKHR(TVkSharedContext(SharedContext).Device, FSwapchain, High(UInt64), LSemaphore, VK_NULL_HANDLE, @FBackBuffers[FCurrentBackBufferIndex].Index) <> VK_SUCCESS then
      begin
        TVkSharedContext(SharedContext).VkInterface.DestroySemaphore(TVkSharedContext(SharedContext).Device, LSemaphore, nil);
        Exit;
      end;
    end;
    LGrBackendSemaphore := TGrBackendSemaphore.Create;
    LGrBackendSemaphore.InitVulkan(LSemaphore);
    FBackBufferSurfaces[FBackBuffers[FCurrentBackBufferIndex].Index].Wait([LGrBackendSemaphore]);
    Result := TSkSurface(FBackBufferSurfaces[FBackBuffers[FCurrentBackBufferIndex].Index]);
  finally
    if Result = nil then
      SharedContext.EndContext;
  end;
end;

class function TVkCanvas.IsSupported: Boolean;
var
  LCount: Cardinal;
  LCreateInstance: PFN_vkCreateInstance;
  LDestroyInstance: PFN_vkDestroyInstance;
  LEnumeratePhysicalDevices: PFN_vkEnumeratePhysicalDevices;
  LGetInstanceProcAddr: PFN_vkGetInstanceProcAddr;
  LInstance: VkInstance;
  LInstanceCreateInfo: VkInstanceCreateInfo;
  LLibraryHandle: HMODULE;
begin
  LLibraryHandle := VkLoadLibrary;
  if LLibraryHandle = 0 then
    Exit(False);
  try
    LGetInstanceProcAddr := PFN_vkGetInstanceProcAddr(GetProcAddress(LLibraryHandle, 'vkGetInstanceProcAddr'));
    if not Assigned(LGetInstanceProcAddr) then
      Exit(False);
    LCreateInstance := PFN_vkCreateInstance(LGetInstanceProcAddr(VK_NULL_HANDLE, 'vkCreateInstance'));
    if not Assigned(LCreateInstance) then
      Exit(False);
    LInstanceCreateInfo.sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    LInstanceCreateInfo.pNext                   := nil;
    LInstanceCreateInfo.flags                   := 0;
    LInstanceCreateInfo.pApplicationInfo        := nil;
    LInstanceCreateInfo.enabledLayerCount       := 0;
    LInstanceCreateInfo.ppEnabledLayerNames     := nil;
    LInstanceCreateInfo.enabledExtensionCount   := 0;
    LInstanceCreateInfo.ppEnabledExtensionNames := nil;
    if LCreateInstance(@LInstanceCreateInfo, nil, @LInstance) <> VK_SUCCESS then
      Exit(False);
    LDestroyInstance := PFN_vkDestroyInstance(LGetInstanceProcAddr(LInstance, 'vkDestroyInstance'));
    // The function 'vkGetInstanceProcAddr' should not fail to retrieve the
    // address of 'vkDestroyInstance' once the Vulkan instance has been
    // successfully created. However, we cannot fully trust the manufacturers'
    // implementations.
    if not Assigned(LDestroyInstance) then
      Exit(False);
    try
      LEnumeratePhysicalDevices := PFN_vkEnumeratePhysicalDevices(LGetInstanceProcAddr(LInstance, 'vkEnumeratePhysicalDevices'));
      Result := (Assigned(LEnumeratePhysicalDevices)) and (LEnumeratePhysicalDevices(LInstance, @LCount, nil) = VK_SUCCESS) and (LCount > 0);
    finally
      LDestroyInstance(LInstance, nil);
    end;
  finally
    {$IFDEF MSWINDOWS}Winapi.Windows{$ELSE}System.SysUtils{$ENDIF}.FreeLibrary(LLibraryHandle);
  end;
end;

procedure TVkCanvas.Resized;
begin
  inherited;
  if FSwapchain <> VK_NULL_HANDLE then
    CreateSwapchain;
end;

procedure TVkCanvas.SwapBuffers(const AContextHandle: THandle);
var
  LGrBackendSemaphore: IGrBackendSemaphore;
  LPresentInfo: VkPresentInfoKHR;
  LPresentState: IGrBackendSurfaceMutableState;
begin
  inherited;
  LGrBackendSemaphore := TGrBackendSemaphore.Create;
  LGrBackendSemaphore.InitVulkan(FBackBuffers[FCurrentBackBufferIndex].Semaphore);
  LPresentState := TGrBackendSurfaceMutableState.Create(VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, TVkSharedContext(SharedContext).PresentQueueIndex);
  FBackBufferSurfaces[FBackBuffers[FCurrentBackBufferIndex].Index].FlushAndSubmit([LGrBackendSemaphore], LPresentState);
  LPresentInfo.sType              := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
  LPresentInfo.pNext              := nil;
  LPresentInfo.waitSemaphoreCount := 1;
  LPresentInfo.pWaitSemaphores    := @FBackBuffers[FCurrentBackBufferIndex].Semaphore;
  LPresentInfo.swapchainCount     := 1;
  LPresentInfo.pSwapchains        := @FSwapchain;
  LPresentInfo.pImageIndices      := @FBackBuffers[FCurrentBackBufferIndex].Index;
  LPresentInfo.pResults           := nil;
  TVkSharedContext(SharedContext).VkInterface.QueuePresentKHR(TVkSharedContext(SharedContext).PresentQueue, @LPresentInfo);
  SharedContext.EndContext;
end;

{ TVkInterface }

procedure TVkInterface.AcquireDestroyInstanceProc(const AInstance: VkInstance);
begin
  GetInstanceProc(AInstance, 'vkDestroyInstance', FDestroyInstance);
end;

procedure TVkInterface.AcquireDeviceProcs(const ADevice: VkDevice);
begin
  GetDeviceProc(ADevice, 'vkCreateSemaphore', FCreateSemaphore);
  GetDeviceProc(ADevice, 'vkDestroySemaphore', FDestroySemaphore);
  GetDeviceProc(ADevice, 'vkDeviceWaitIdle', FDeviceWaitIdle);
  GetDeviceProc(ADevice, 'vkGetDeviceQueue', FGetDeviceQueue);
  GetDeviceProc(ADevice, 'vkQueueWaitIdle', FQueueWaitIdle);
  GetDeviceProc(ADevice, 'vkCreateSwapchainKHR', FCreateSwapchainKHR);
  GetDeviceProc(ADevice, 'vkDestroySwapchainKHR', FDestroySwapchainKHR);
  GetDeviceProc(ADevice, 'vkGetSwapchainImagesKHR', FGetSwapchainImagesKHR);
  GetDeviceProc(ADevice, 'vkAcquireNextImageKHR', FAcquireNextImageKHR);
  GetDeviceProc(ADevice, 'vkQueuePresentKHR', FQueuePresentKHR);
end;

procedure TVkInterface.AcquireExtensionProcs(const AInstance: VkInstance;
  const APhysicalDeviceApiVersion: Cardinal;
  const AGrVkExtensions: IGrVkExtensions);
begin
  if APhysicalDeviceApiVersion >= VK_MAKE_VERSION(1, 1, 0) then
  begin
    GetInstanceProc(AInstance, 'vkGetPhysicalDeviceFeatures2', FGetPhysicalDeviceFeatures2);
  end
  else
  begin
    if AGrVkExtensions.HasExtension(VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME, 1) then
      GetInstanceProc(AInstance, 'vkGetPhysicalDeviceFeatures2KHR', FGetPhysicalDeviceFeatures2);
  end;
end;

procedure TVkInterface.AcquireInstanceProcs(const AInstance: VkInstance);
begin
  GetInstanceProc(AInstance, 'vkCreateDevice', FCreateDevice);
  GetInstanceProc(AInstance, 'vkDestroyDevice', FDestroyDevice);
  GetInstanceProc(AInstance, 'vkEnumerateDeviceExtensionProperties', FEnumerateDeviceExtensionProperties);
  GetInstanceProc(AInstance, 'vkEnumeratePhysicalDevices', FEnumeratePhysicalDevices);
  GetInstanceProc(AInstance, 'vkGetDeviceProcAddr', FGetDeviceProcAddr);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceFeatures', FGetPhysicalDeviceFeatures);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceProperties', FGetPhysicalDeviceProperties);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceQueueFamilyProperties', FGetPhysicalDeviceQueueFamilyProperties);
  GetInstanceProc(AInstance, 'vkDestroySurfaceKHR', FDestroySurfaceKHR);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR', FGetPhysicalDeviceSurfaceCapabilitiesKHR);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceSurfaceFormatsKHR', FGetPhysicalDeviceSurfaceFormatsKHR);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceSurfacePresentModesKHR', FGetPhysicalDeviceSurfacePresentModesKHR);
  {$IF DEFINED(ANDROID)}
  GetInstanceProc(AInstance, 'vkCreateAndroidSurfaceKHR', FCreateAndroidSurfaceKHR);
  {$ELSEIF DEFINED(MSWINDOWS)}
  GetInstanceProc(AInstance, 'vkCreateWin32SurfaceKHR', FCreateWin32SurfaceKHR);
  GetInstanceProc(AInstance, 'vkGetPhysicalDeviceWin32PresentationSupportKHR', FGetPhysicalDeviceWin32PresentationSupportKHR);
  {$ENDIF}
end;

procedure TVkInterface.GetDeviceProc(const ADevice: VkDevice;
  const AName: MarshaledAString; out APFN_vkFunction);
begin
  PPointer(@APFN_vkFunction)^ := Pointer(FGetDeviceProcAddr(ADevice, AName));
  if PPointer(@APFN_vkFunction)^ = nil then
    raise EVkError.CreateFmt('Could not get the address of the "%s" device function.', [AName]) at ReturnAddress;
end;

procedure TVkInterface.GetInstanceProc(const AInstance: VkInstance;
  const AName: MarshaledAString; out APFN_vkFunction;
  const ARaiseIfNotExist: Boolean);
begin
  PPointer(@APFN_vkFunction)^ := Pointer(FGetInstanceProcAddr(AInstance, AName));
  if (ARaiseIfNotExist) and (PPointer(@APFN_vkFunction)^ = nil) then
    raise EVkError.CreateFmt('Could not get the address of the "%s" instance function.', [AName]) at ReturnAddress;
end;

procedure TVkInterface.Initialize(const ALibraryHandle: HMODULE);
begin
  FGetInstanceProcAddr := GetProcAddress(ALibraryHandle, 'vkGetInstanceProcAddr');
  if not Assigned(FGetInstanceProcAddr) then
    raise EGrCanvas.Create('Could not get the address of the "vkGetInstanceProcAddr" function.');
  GetInstanceProc(VK_NULL_HANDLE, 'vkCreateInstance', FCreateInstance);
  GetInstanceProc(VK_NULL_HANDLE, 'vkEnumerateInstanceExtensionProperties', FEnumerateInstanceExtensionProperties);
  GetInstanceProc(VK_NULL_HANDLE, 'vkEnumerateInstanceVersion', FEnumerateInstanceVersion, False);
end;

{ TVkSharedContext }

constructor TVkSharedContext.Create;
const
  QueuePriorities: array[0..0] of Single = (0);
var
  I: Integer;
  J: Integer;
  LApplicationInfo: VkApplicationInfo;
  LBlend: PVkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
  LCount: Cardinal;
  LDeviceCreateInfo: VkDeviceCreateInfo;
  LDeviceExtensionNames: TArray<MarshaledAString>;
  LDeviceExtensions: TArray<VkExtensionProperties>;
  LDeviceQueueCreateInfo: array[0..1] of VkDeviceQueueCreateInfo;
  LExtensions: IGrVkExtensions;
  LGetProc: TGrVkGetProc;
  LGrVkBackendContext: TGrVkBackendContext;
  LHasKHRBufferDeviceAddress: Boolean;
  LInstanceCreateInfo: VkInstanceCreateInfo;
  LInstanceExtensionNames: TArray<MarshaledAString>;
  LInstanceExtensions: TArray<VkExtensionProperties>;
  LMaxApiVersion: Cardinal;
  LPhysicalDeviceApiVersion: Cardinal;
  LPhysicalDeviceFeatures: PVkPhysicalDeviceFeatures;
  LPhysicalDeviceFeatures2: PVkPhysicalDeviceFeatures2;
  LPhysicalDeviceProperties: VkPhysicalDeviceProperties;
  LPhysicalDevices: TArray<VkPhysicalDevice>;
  LQueueFamilyProperties: TArray<VkQueueFamilyProperties>;
  LTailNext: PPointer;
  LYcbcr: PVkPhysicalDeviceSamplerYcbcrConversionFeatures;
begin
  inherited;
  FLibraryHandle := VkLoadLibrary;
  if FLibraryHandle = 0 then
    raise EGrCanvas.Create('Could not load Vulkan library.');
  try
    FVkInterface.Initialize(FLibraryHandle);
    if not Assigned(FVkInterface.EnumerateInstanceVersion) then
      LMaxApiVersion := VK_MAKE_VERSION(1, 0, 0)
    else
    begin
      VkCheckSuccess(FVkInterface.EnumerateInstanceVersion(@LMaxApiVersion));
      if LMaxApiVersion > VK_MAKE_VERSION(1, 1, 0) then
        LMaxApiVersion := VK_MAKE_VERSION(1, 1, 0);
    end;
    VkCheckSuccess(FVkInterface.EnumerateInstanceExtensionProperties(nil, @LCount, nil));
    SetLength(LInstanceExtensions, LCount);
    VkCheckSuccess(FVkInterface.EnumerateInstanceExtensionProperties(nil, @LCount, Pointer(LInstanceExtensions)));
    SetLength(LInstanceExtensionNames, Length(LInstanceExtensions));
    J := 0;
    for I := 0 to Length(LInstanceExtensions) - 1 do
    begin
      if StrLComp(@LInstanceExtensions[I].extensionName, 'VK_KHX', 6) <> 0 then
      begin
        LInstanceExtensionNames[J] := @LInstanceExtensions[I].extensionName;
        Inc(J);
      end;
    end;
    SetLength(LInstanceExtensionNames, J);
    LApplicationInfo.sType                      := VK_STRUCTURE_TYPE_APPLICATION_INFO;
    LApplicationInfo.pNext                      := nil;
    LApplicationInfo.pApplicationName           := nil;
    LApplicationInfo.applicationVersion         := 0;
    LApplicationInfo.pEngineName                := nil;
    LApplicationInfo.engineVersion              := 0;
    LApplicationInfo.apiVersion                 := LMaxApiVersion;
    LInstanceCreateInfo.sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    LInstanceCreateInfo.pNext                   := nil;
    LInstanceCreateInfo.flags                   := 0;
    LInstanceCreateInfo.pApplicationInfo        := @LApplicationInfo;
    LInstanceCreateInfo.enabledLayerCount       := 0;
    LInstanceCreateInfo.ppEnabledLayerNames     := nil;
    LInstanceCreateInfo.enabledExtensionCount   := Length(LInstanceExtensionNames);
    LInstanceCreateInfo.ppEnabledExtensionNames := Pointer(LInstanceExtensionNames);
    VkCheckSuccess(FVkInterface.CreateInstance(@LInstanceCreateInfo, nil, @FInstance));
    FVkInterface.AcquireDestroyInstanceProc(FInstance);
    try
      FVkInterface.AcquireInstanceProcs(FInstance);
      LGetProc :=
        function (const AName: MarshaledAString; const AInstance: GrVkInstance; const ADevice: GrVkDevice): Pointer
        begin
          if ADevice <> VK_NULL_HANDLE then
            Result := Pointer(FVkInterface.GetDeviceProcAddr(ADevice, AName))
          else
            Result := Pointer(FVkInterface.GetInstanceProcAddr(AInstance, AName));
        end;
      VkCheckSuccess(FVkInterface.EnumeratePhysicalDevices(FInstance, @LCount, nil));
      if LCount = 0 then
        raise EVkError.Create('No compatible graphics card found for Vulkan rendering.');
      SetLength(LPhysicalDevices, LCount);
      VkCheckSuccess(FVkInterface.EnumeratePhysicalDevices(FInstance, @LCount, Pointer(LPhysicalDevices)));
      FPhysicalDevice := LPhysicalDevices[0];
      FVkInterface.GetPhysicalDeviceProperties(FPhysicalDevice, @LPhysicalDeviceProperties);
      LPhysicalDeviceApiVersion := Min(LPhysicalDeviceProperties.apiVersion, LMaxApiVersion);
      FVkInterface.GetPhysicalDeviceQueueFamilyProperties(FPhysicalDevice, @LCount, nil);
      if LCount = 0 then
        raise EVkError.Create('No queue families were found for the specified physical device.');
      SetLength(LQueueFamilyProperties, LCount);
      FVkInterface.GetPhysicalDeviceQueueFamilyProperties(FPhysicalDevice, @LCount, Pointer(LQueueFamilyProperties));
      FGraphicsQueueIndex := LCount;
      for I := 0 to Length(LQueueFamilyProperties) - 1 do
      begin
        if (LQueueFamilyProperties[I].queueFlags and VK_QUEUE_GRAPHICS_BIT) <> 0 then
        begin
          FGraphicsQueueIndex := I;
          Break;
        end;
      end;
      if FGraphicsQueueIndex = LCount then
        raise EVkError.Create('The physical device does not support graphics operations.');
      {$IF DEFINED(MSWINDOWS)}
      FPresentQueueIndex := LCount;
      for I := 0 to Length(LQueueFamilyProperties) - 1 do
      begin
        if FVkInterface.GetPhysicalDeviceWin32PresentationSupportKHR(FPhysicalDevice, I) <> VK_FALSE then
        begin
          FPresentQueueIndex := I;
          Break;
        end;
      end;
      if FPresentQueueIndex = LCount then
        raise EVkError.Create('The physical device does not support presentation operations.');
      {$ELSE}
      FPresentQueueIndex := FGraphicsQueueIndex;
      {$ENDIF}
      VkCheckSuccess(FVkInterface.EnumerateDeviceExtensionProperties(FPhysicalDevice, nil, @LCount, nil));
      SetLength(LDeviceExtensions, LCount);
      VkCheckSuccess(FVkInterface.EnumerateDeviceExtensionProperties(FPhysicalDevice, nil, @LCount, Pointer(LDeviceExtensions)));
      LHasKHRBufferDeviceAddress := False;
      for I := 0 to Length(LDeviceExtensions) - 1 do
      begin
        if StrComp(@LDeviceExtensions[I].extensionName, 'VK_KHR_buffer_device_address') = 0 then
        begin
          LHasKHRBufferDeviceAddress := True;
          Break;
        end;
      end;
      SetLength(LDeviceExtensionNames, Length(LDeviceExtensions));
      J := 0;
      for I := 0 to Length(LDeviceExtensions) - 1 do
      begin
        if (StrLComp(@LDeviceExtensions[I].extensionName, 'VK_KHX', 6) <> 0) and (StrLComp(@LDeviceExtensions[I].extensionName, 'VK_NVX', 6) <> 0) then
        begin
          if (not LHasKHRBufferDeviceAddress) or (StrComp(@LDeviceExtensions[I].extensionName, 'VK_EXT_buffer_device_address') <> 0) then
          begin
            LDeviceExtensionNames[J] := @LDeviceExtensions[I].extensionName;
            Inc(J);
          end;
        end;
      end;
      SetLength(LDeviceExtensionNames, J);
      LExtensions := TGrVkExtensions.Create;
      LExtensions.Init(LGetProc, FInstance, FPhysicalDevice, LInstanceExtensionNames, LDeviceExtensionNames);
      FVkInterface.AcquireExtensionProcs(FInstance, LPhysicalDeviceApiVersion, LExtensions);
      FFeatures.sType := VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2;
      LPhysicalDeviceFeatures  := @FFeatures.features;
      try
        if Assigned(FVkInterface.GetPhysicalDeviceFeatures2) then
        begin
          LPhysicalDeviceFeatures2 := @FFeatures;
          LTailNext := @FFeatures.pNext;
          if LExtensions.HasExtension(VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME, 2) then
          begin
            GetMem(LBlend, SizeOf(VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT));
            LBlend.sType := VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT;
            LBlend.pNext := nil;
            LTailNext^ := LBlend;
            LTailNext  := @LBlend.pNext;
          end;
          if LExtensions.HasExtension(VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME, 1) then
          begin
            GetMem(LYcbcr, SizeOf(VkPhysicalDeviceSamplerYcbcrConversionFeatures));
            LYcbcr.sType                  := VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES;
            LYcbcr.pNext                  := nil;
            LYcbcr.samplerYcbcrConversion := VK_TRUE;
            LTailNext^ := LYcbcr;
            //LTailNext  := @LYcbcr.pNext;
          end;
          FVkInterface.GetPhysicalDeviceFeatures2(FPhysicalDevice, LPhysicalDeviceFeatures2);
        end
        else
        begin
          LPhysicalDeviceFeatures2 := nil;
          FVkInterface.GetPhysicalDeviceFeatures(FPhysicalDevice, LPhysicalDeviceFeatures);
        end;
        FFeatures.features.robustBufferAccess := VK_FALSE;
        LDeviceCreateInfo.sType                    := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
        LDeviceCreateInfo.flags                    := 0;
        LDeviceCreateInfo.enabledLayerCount        := 0;
        LDeviceCreateInfo.ppEnabledLayerNames      := nil;
        LDeviceCreateInfo.enabledExtensionCount    := Length(LDeviceExtensionNames);
        LDeviceCreateInfo.ppEnabledExtensionNames  := Pointer(LDeviceExtensionNames);
        LDeviceQueueCreateInfo[0].sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        LDeviceQueueCreateInfo[0].pNext            := nil;
        LDeviceQueueCreateInfo[0].flags            := 0;
        LDeviceQueueCreateInfo[0].queueFamilyIndex := FGraphicsQueueIndex;
        LDeviceQueueCreateInfo[0].queueCount       := 1;
        LDeviceQueueCreateInfo[0].pQueuePriorities := @QueuePriorities;
        if FGraphicsQueueIndex = FPresentQueueIndex then
          LDeviceCreateInfo.queueCreateInfoCount := 1
        else
        begin
          LDeviceQueueCreateInfo[1].sType            := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
          LDeviceQueueCreateInfo[1].pNext            := nil;
          LDeviceQueueCreateInfo[1].flags            := 0;
          LDeviceQueueCreateInfo[1].queueFamilyIndex := FPresentQueueIndex;
          LDeviceQueueCreateInfo[1].queueCount       := 1;
          LDeviceQueueCreateInfo[1].pQueuePriorities := @QueuePriorities;
          LDeviceCreateInfo.queueCreateInfoCount     := 2;
        end;
        LDeviceCreateInfo.pQueueCreateInfos := @LDeviceQueueCreateInfo;
        LDeviceCreateInfo.pNext             := LPhysicalDeviceFeatures2;
        if LDeviceCreateInfo.pNext = nil then
          LDeviceCreateInfo.pEnabledFeatures := LPhysicalDeviceFeatures
        else
          LDeviceCreateInfo.pEnabledFeatures := nil;
        VkCheckSuccess(FVkInterface.CreateDevice(FPhysicalDevice, @LDeviceCreateInfo, nil, @FDevice));
        try
          FVkInterface.AcquireDeviceProcs(FDevice);
          FVkInterface.GetDeviceQueue(FDevice, FGraphicsQueueIndex, 0, @FGraphicsQueue);
          FVkInterface.GetDeviceQueue(FDevice, FPresentQueueIndex, 0, @FPresentQueue);
          LGrVkBackendContext.Instance                := FInstance;
          LGrVkBackendContext.PhysicalDevice          := FPhysicalDevice;
          LGrVkBackendContext.Device                  := FDevice;
          LGrVkBackendContext.Queue                   := FGraphicsQueue;
          LGrVkBackendContext.GraphicsQueueIndex      := FGraphicsQueueIndex;
          LGrVkBackendContext.MaxApiVersion           := LMaxApiVersion;
          LGrVkBackendContext.Extensions              := LExtensions;
          LGrVkBackendContext.PhysicalDeviceFeatures  := LPhysicalDeviceFeatures;
          LGrVkBackendContext.PhysicalDeviceFeatures2 := LPhysicalDeviceFeatures2;
          LGrVkBackendContext.GetProc                 := LGetProc;
          LGrVkBackendContext.ProtectedContext        := False;
          FGrDirectContext := TGrDirectContext.MakeVulkan(LGrVkBackendContext);
          if FGrDirectContext = nil then
            raise EGrCanvas.Create('Could not create shared direct context.');
        except
          FVkInterface.DestroyDevice(FDevice, nil);
          raise;
        end;
      except
        DestroyFeatures;
        raise;
      end;
    except
      FVkInterface.DestroyInstance(FInstance, nil);
      raise;
    end;
  except
    {$IFDEF MSWINDOWS}Winapi.Windows{$ELSE}System.SysUtils{$ENDIF}.FreeLibrary(FLibraryHandle);
    raise;
  end;
end;

procedure TVkSharedContext.DestroyContext;
begin
  inherited;
  FVkInterface.DestroyDevice(FDevice, nil);
  DestroyFeatures;
  FVkInterface.DestroyInstance(FInstance, nil);
  {$IFDEF MSWINDOWS}Winapi.Windows{$ELSE}System.SysUtils{$ENDIF}.FreeLibrary(FLibraryHandle);
end;

procedure TVkSharedContext.DestroyFeatures;
type
  PVkCommon = ^TVkCommon;
  TVkCommon = record
    sType: VkStructureType;
    pNext: Pointer;
  end;
var
  LCurrent: Pointer;
  LNext: Pointer;
begin
  LNext := FFeatures.pNext;
  while LNext <> nil do
  begin
    LCurrent := LNext;
    LNext    := PVkCommon(LCurrent).pNext;
    FreeMem(LCurrent);
  end;
end;

function TVkSharedContext.GetTextureColorType: TSkColorType;
begin
  Result := {$IFDEF MSWINDOWS}TSkColorType.BGRA8888{$ELSE}TSkColorType.RGBA8888{$ENDIF};
end;

function TVkSharedContext.GetTextureOrigin: TGrSurfaceOrigin;
begin
  Result := TGrSurfaceOrigin.TopLeft;
end;

{$HPPEMIT END '#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_SKIA_CANVAS_VULKAN)'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::Vulkan::TVkInterface;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::Vulkan::TVkSharedContextCustom;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::Vulkan::GlobalUseSkiaVulkanFifoKHR;'}
{$HPPEMIT END '#endif'}

initialization
  GlobalUseSkiaVulkanFifoKHR := {$IFDEF ANDROID}True{$ELSE}False{$ENDIF};
  RegisterSkiaRenderCanvas(TVkCanvas, True,
    function: Boolean
    begin
      Result := GlobalUseVulkan and TVkCanvas.IsSupported;
    end);
{$ELSE}
implementation
{$ENDIF}
end.
