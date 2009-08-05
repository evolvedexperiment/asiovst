unit DAV_Classes;

// based on code found in the GLScene (see www.glscene.org)

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Common;

type
  TDAVUpdateAbleObject = class(TPersistent)
  private
    FOwner : TPersistent;
    FUpdating : Integer;
    FOnNotifyChange : TNotifyEvent;
  public
    constructor Create(AOwner: TPersistent); virtual;

    procedure NotifyChange(Sender : TObject); virtual;
    function GetOwner : TPersistent; override;

    property Updating : Integer read FUpdating;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner : TPersistent read FOwner;
    property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
 end;

  TDAVCadenceAbleComponent = class (TComponent)
  public
    {$IFNDEF DELPHI_5_UP}
    procedure RemoveFreeNotification(AComponent: TComponent);
    {$ENDIF}
  end;

  TDAVUpdateAbleComponent = class (TDAVCadenceAbleComponent)
  public
    procedure NotifyChange(Sender : TObject); virtual;
  end;


implementation

{ TDAVUpdateAbleObject }

constructor TDAVUpdateAbleObject.Create(AOwner: TPersistent);
begin
 inherited Create;
 FOwner := AOwner;
end;

procedure TDAVUpdateAbleObject.BeginUpdate;
begin
 Inc(FUpdating);
end;

procedure TDAVUpdateAbleObject.EndUpdate;
begin
 Dec(FUpdating);
 if FUpdating <= 0 then
  begin
   Assert(FUpdating = 0);
   NotifyChange(Self);
  end;
end;

function TDAVUpdateAbleObject.GetOwner: TPersistent;
begin
 Result := Owner;
end;

procedure TDAVUpdateAbleObject.NotifyChange(Sender: TObject);
begin
 if (FUpdating = 0) and Assigned(Owner) then
  begin
   if Owner is TDAVUpdateAbleObject
    then TDAVUpdateAbleObject(Owner).NotifyChange(Self)
    else
   if Owner is TDAVUpdateAbleComponent
    then TDAVUpdateAbleComponent(Owner).NotifyChange(Self);
   if Assigned(FOnNotifyChange)
    then FOnNotifyChange(Self);
  end;
end;

{ TDAVCadenceAbleComponent }

procedure TDAVCadenceAbleComponent.RemoveFreeNotification(
  AComponent: TComponent);
begin
 Notification(AComponent, opRemove);
end;

{ TDAVUpdateAbleComponent }

procedure TDAVUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
 if Assigned(Owner) then
 if (Owner is TDAVUpdateAbleComponent) then
    (Owner as TDAVUpdateAbleComponent).NotifyChange(Self);
end;

end.
