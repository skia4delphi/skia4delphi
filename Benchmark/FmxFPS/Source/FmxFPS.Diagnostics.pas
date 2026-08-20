{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2026 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit FmxFPS.Diagnostics;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.JSON;

type
  { IBenchmarkDiagnostics }

  /// <summary>
  ///   Everything the benchmark measures. The average frame rate on its own hides stutter: a frame stream
  ///   alternating between 10 ms and 22 ms averages out to a healthy looking 60 per second while looking
  ///   visibly broken. What separates the two is the tail of the frame interval distribution and how irregular
  ///   the cadence is, so those are measured as well.
  /// </summary>
  IBenchmarkDiagnostics = interface
    ['{6E1D9A2B-4C77-4E58-9C0E-2A5C4B7F3D81}']
    function GetElapsedMilliseconds: Int64;
    function GetPaintCount: Int64;
    /// <summary>Call once per painted frame. The timed window starts at the first call.</summary>
    procedure RecordFrame;
    /// <summary>Discards anything measured so far and arms the measurement.</summary>
    procedure Start;
    /// <summary>Stops measuring. Anything happening after this is not counted.</summary>
    procedure Stop;
    /// <summary>The measurements, for the dialog shown when the benchmark runs without <c>-ci</c>.</summary>
    function Summary: string;
    /// <summary>Adds the measurements to the result document.</summary>
    procedure WriteTo(const ARootObject: TJSONObject);
    /// <summary>Time since the first recorded frame.</summary>
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property PaintCount: Int64 read GetPaintCount;
  end;

function CreateBenchmarkDiagnostics: IBenchmarkDiagnostics;

implementation

uses
  { Delphi }
  System.SysUtils, System.Math, System.Diagnostics, System.Generics.Collections,
  System.Generics.Defaults, FMX.Platform
  {$IF CompilerVersion >= 37}
  , FMX.DisplayLink
  {$ENDIF};

const
  /// <summary>A frame taking longer than this multiple of the display period reads as a hitch.</summary>
  LongFrameFactor = 1.5;
  DefaultRefreshRate = 60;

type
  { TIntervalSeries }

  /// <summary>A growing series of intervals, in milliseconds, with the statistics that describe its shape.</summary>
  TIntervalSeries = record
  private
    FCount: Integer;
    FLastTicks: Int64;
    FValues: TArray<Double>;
    function Percentile(const APercent: Integer): Double;
  public
    procedure Reset;
    procedure Add(const AMilliseconds: Double);
    /// <summary>Adds the time elapsed since the previous call. The first call only sets the origin.</summary>
    procedure AddElapsed;
    /// <summary>Number of intervals longer than <c>APeriod</c> times <c>LongFrameFactor</c>.</summary>
    function CountLongerThan(const APeriod: Double): Integer;
    /// <summary>Mean absolute difference between consecutive intervals, that is, how irregular the cadence is.</summary>
    function Jitter: Double;
    function Max: Double;
    function Mean: Double;
    function Median: Double;
    function P95: Double;
    function P99: Double;
    property Count: Integer read FCount;
  end;

  { TBenchmarkDiagnostics }

  TBenchmarkDiagnostics = class(TInterfacedObject, IBenchmarkDiagnostics)
  private
    FFrames: TIntervalSeries;
    FPaintCount: Int64;
    FRunning: Boolean;
    FStopwatch: TStopwatch;
    {$IF CompilerVersion >= 37}
    FDisplayLinkService: IFMXDisplayLinkService;
    FLastDisplayLinkTime: Double;
    FTicks: TIntervalSeries;
    procedure DisplayLinkUpdated(const ATime, ADeltaTime: Double);
    {$ENDIF}
    /// <summary>
    ///   The period the display actually ran at. Taken from the display link when there is one, so that the
    ///   long frame threshold stays meaningful on any refresh rate, and from the frames themselves otherwise.
    /// </summary>
    function DisplayPeriod: Double;
    function FramesPerSecond: Double;
    function HasTicks: Boolean;
    function TicksPerSecond: Double;
  public
    destructor Destroy; override;
    function GetElapsedMilliseconds: Int64;
    function GetPaintCount: Int64;
    procedure RecordFrame;
    procedure Start;
    procedure Stop;
    function Summary: string;
    procedure WriteTo(const ARootObject: TJSONObject);
  end;

function CreateBenchmarkDiagnostics: IBenchmarkDiagnostics;
begin
  Result := TBenchmarkDiagnostics.Create;
end;

{ TIntervalSeries }

procedure TIntervalSeries.Reset;
begin
  FCount := 0;
  FLastTicks := 0;
end;

procedure TIntervalSeries.Add(const AMilliseconds: Double);
begin
  if FCount = Length(FValues) then
    SetLength(FValues, System.Math.Max(1024, FCount * 2));
  FValues[FCount] := AMilliseconds;
  Inc(FCount);
end;

procedure TIntervalSeries.AddElapsed;
var
  LTicks: Int64;
begin
  LTicks := TStopwatch.GetTimeStamp;
  if FLastTicks <> 0 then
    Add(((LTicks - FLastTicks) * MSecsPerSec) / TStopwatch.Frequency);
  FLastTicks := LTicks;
end;

function TIntervalSeries.Percentile(const APercent: Integer): Double;
var
  LSorted: TArray<Double>;
begin
  if FCount = 0 then
    Exit(0);
  LSorted := Copy(FValues, 0, FCount);
  TArray.Sort<Double>(LSorted);
  Result := LSorted[System.Math.Min(FCount - 1, (FCount * APercent) div 100)];
end;

function TIntervalSeries.CountLongerThan(const APeriod: Double): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FCount - 1 do
    if FValues[I] > APeriod * LongFrameFactor then
      Inc(Result);
end;

function TIntervalSeries.Jitter: Double;
var
  I: Integer;
  LSum: Double;
begin
  if FCount < 2 then
    Exit(0);
  LSum := 0;
  for I := 1 to FCount - 1 do
    LSum := LSum + Abs(FValues[I] - FValues[I - 1]);
  Result := LSum / (FCount - 1);
end;

function TIntervalSeries.Max: Double;
begin
  Result := Percentile(100);
end;

function TIntervalSeries.Mean: Double;
var
  I: Integer;
  LSum: Double;
begin
  if FCount = 0 then
    Exit(0);
  LSum := 0;
  for I := 0 to FCount - 1 do
    LSum := LSum + FValues[I];
  Result := LSum / FCount;
end;

function TIntervalSeries.Median: Double;
begin
  Result := Percentile(50);
end;

function TIntervalSeries.P95: Double;
begin
  Result := Percentile(95);
end;

function TIntervalSeries.P99: Double;
begin
  Result := Percentile(99);
end;

{ TBenchmarkDiagnostics }

destructor TBenchmarkDiagnostics.Destroy;
begin
  Stop;
  inherited;
end;

{$IF CompilerVersion >= 37}

/// <summary>
///   Counts how often the display link delivers a frame to the animation engine. This is not the paint rate: a
///   display link that lost its lock on the refresh can keep the paint count high while ticking the animations
///   at half the rate, and that is what stutter looks like.
/// </summary>
procedure TBenchmarkDiagnostics.DisplayLinkUpdated(const ATime, ADeltaTime: Double);
begin
  if not FRunning then
    Exit;
  if FLastDisplayLinkTime > 0 then
    FTicks.Add((ATime - FLastDisplayLinkTime) * MSecsPerSec);
  FLastDisplayLinkTime := ATime;
end;

{$ENDIF}

function TBenchmarkDiagnostics.HasTicks: Boolean;
begin
  {$IF CompilerVersion >= 37}
  Result := FTicks.Count > 0;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TBenchmarkDiagnostics.TicksPerSecond: Double;
begin
  {$IF CompilerVersion >= 37}
  Result := MSecsPerSec / FTicks.Median;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function TBenchmarkDiagnostics.DisplayPeriod: Double;
begin
  {$IF CompilerVersion >= 37}
  if HasTicks then
    Exit(FTicks.Median);
  {$ENDIF}
  if FFrames.Count > 0 then
    Result := FFrames.Median
  else
    Result := MSecsPerSec / DefaultRefreshRate;
end;

function TBenchmarkDiagnostics.FramesPerSecond: Double;
var
  LSeconds: Double;
begin
  LSeconds := FStopwatch.Elapsed.TotalSeconds;
  if LSeconds > 0 then
    Result := FPaintCount / LSeconds
  else
    Result := 0;
end;

function TBenchmarkDiagnostics.GetElapsedMilliseconds: Int64;
begin
  Result := FStopwatch.ElapsedMilliseconds;
end;

function TBenchmarkDiagnostics.GetPaintCount: Int64;
begin
  Result := FPaintCount;
end;

procedure TBenchmarkDiagnostics.Start;
begin
  FFrames.Reset;
  FPaintCount := 0;
  FStopwatch := TStopwatch.Create;
  FRunning := True;
  {$IF CompilerVersion >= 37}
  FTicks.Reset;
  FLastDisplayLinkTime := 0;
  if (FDisplayLinkService <> nil) or
    TPlatformServices.Current.SupportsPlatformService(IFMXDisplayLinkService, FDisplayLinkService) then
  begin
    FDisplayLinkService.Subscribe(DisplayLinkUpdated);
  end;
  {$ENDIF}
end;

procedure TBenchmarkDiagnostics.Stop;
begin
  FRunning := False;
  FStopwatch.Stop;
  {$IF CompilerVersion >= 37}
  if FDisplayLinkService <> nil then
  begin
    FDisplayLinkService.Unsubscribe(DisplayLinkUpdated);
    FDisplayLinkService := nil;
  end;
  {$ENDIF}
end;

procedure TBenchmarkDiagnostics.RecordFrame;
begin
  if not FRunning then
    Exit;
  if FPaintCount = 0 then
    FStopwatch.Start;
  Inc(FPaintCount);
  FFrames.AddElapsed;
end;

function TBenchmarkDiagnostics.Summary: string;
begin
  Result := Format('%g fps', [FramesPerSecond]);
  if FFrames.Count = 0 then
    Exit;
  Result := Result + Format(sLineBreak + 'frame ms: median %.1f, p95 %.1f, p99 %.1f, max %.1f, jitter %.1f',
    [FFrames.Median, FFrames.P95, FFrames.P99, FFrames.Max, FFrames.Jitter]);
  if HasTicks then
    Result := Result + Format(sLineBreak + 'display link: %.1f ticks/s, jitter %.1f ms',
      [TicksPerSecond, {$IF CompilerVersion >= 37}FTicks.Jitter{$ELSE}0{$ENDIF}]);
end;

procedure TBenchmarkDiagnostics.WriteTo(const ARootObject: TJSONObject);
var
  LLongFrames: Integer;
begin
  ARootObject.AddPair('fps', TJSONNumber.Create(FramesPerSecond));
  ARootObject.AddPair('paint_count', TJSONNumber.Create(FPaintCount));
  ARootObject.AddPair('duration_seconds', TJSONNumber.Create(FStopwatch.Elapsed.TotalSeconds));
  if FFrames.Count = 0 then
    Exit;
  ARootObject.AddPair('frame_ms_mean', TJSONNumber.Create(FFrames.Mean));
  ARootObject.AddPair('frame_ms_median', TJSONNumber.Create(FFrames.Median));
  ARootObject.AddPair('frame_ms_p95', TJSONNumber.Create(FFrames.P95));
  ARootObject.AddPair('frame_ms_p99', TJSONNumber.Create(FFrames.P99));
  ARootObject.AddPair('frame_ms_max', TJSONNumber.Create(FFrames.Max));
  ARootObject.AddPair('frame_ms_jitter', TJSONNumber.Create(FFrames.Jitter));
  {$IF CompilerVersion >= 37}
  if HasTicks then
  begin
    ARootObject.AddPair('display_link_ticks', TJSONNumber.Create(FTicks.Count));
    ARootObject.AddPair('display_link_ticks_per_sec', TJSONNumber.Create(TicksPerSecond));
    ARootObject.AddPair('display_link_ms_median', TJSONNumber.Create(FTicks.Median));
    ARootObject.AddPair('display_link_ms_p95', TJSONNumber.Create(FTicks.P95));
    ARootObject.AddPair('display_link_ms_jitter', TJSONNumber.Create(FTicks.Jitter));
  end;
  {$ENDIF}
  LLongFrames := FFrames.CountLongerThan(DisplayPeriod);
  ARootObject.AddPair('long_frames', TJSONNumber.Create(LLongFrames));
  ARootObject.AddPair('long_frames_percent', TJSONNumber.Create((LLongFrames * 100) / FFrames.Count));
end;

end.
