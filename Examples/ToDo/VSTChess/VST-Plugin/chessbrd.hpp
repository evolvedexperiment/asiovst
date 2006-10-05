// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ChessBrd.pas' rev: 3.00

#ifndef ChessBrdHPP
#define ChessBrdHPP
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Controls.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Chessbrd
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EChessException;
class PASCALIMPLEMENTATION EChessException : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ __fastcall EChessException(const System::AnsiString Msg) : Sysutils::Exception(
		Msg) { }
	/* Exception.CreateFmt */ __fastcall EChessException(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EChessException(int Ident, Extended Dummy) : Sysutils::Exception(
		Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EChessException(int Ident, const System::TVarRec * Args, const 
		int Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EChessException(const System::AnsiString Msg, int AHelpContext
		) : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EChessException(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EChessException(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EChessException(int Ident, const System::TVarRec * Args
		, const int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext
		) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EChessException(void) { }
	
};

enum Square { None, A8, B8, C8, D8, E8, F8, G8, H8, A7, B7, C7, D7, E7, F7, G7, H7, A6, B6, C6, D6, 
	E6, F6, G6, H6, A5, B5, C5, D5, E5, F5, G5, H5, A4, B4, C4, D4, E4, F4, G4, H4, A3, B3, C3, D3, E3, 
	F3, G3, H3, A2, B2, C2, D2, E2, F2, G2, H2, A1, B1, C1, D1, E1, F1, G1, H1 };

enum DisplayCoords { West, North, East, South };

enum CanStillCastle { WhiteKingSide, WhiteQueenSide, BlackKingSide, BlackQueenSide };

typedef Set<CanStillCastle, WhiteKingSide, BlackQueenSide>  CastleSet;

typedef Set<DisplayCoords, West, South>  CoordSet;

struct MoveInfo
{
	System::AnsiString position;
	CastleSet Castling;
	Square OldSquare;
	Square NewSquare;
	Square EnPassant;
} ;

enum pieces { BP, BN, BB, BR, BK, BQ, WP, WN, WB, WR, WQ, WK };

struct gen_rec;
typedef gen_rec *pGenRec;

typedef Square *pSquare;

typedef CastleSet *pCastleSet;

typedef bool *pBoolean;

typedef Classes::TThreadPriority *pThreadPriority;

typedef void __fastcall (__closure *TMoveEvent)(System::TObject* Sender, Square oldSq, Square newSq)
	;

typedef void __fastcall (__closure *TCaptureEvent)(System::TObject* Sender, Square oldSq, Square newSq
	, char CapturedPiece);

typedef void __fastcall (__closure *TOneSquareEvent)(System::TObject* Sender, Square square);

typedef void __fastcall (__closure *TPromotionEvent)(System::TObject* Sender, Square oldSq, Square newSq
	, char &NewPiece);

typedef bool __fastcall (__closure *TMoveFunc)(Square oldsq, Square newsq);

typedef void __fastcall (__closure *TThinkEvent)(System::TObject* Sender, Square &oldsq, Square &newsq
	);

#pragma pack(push, 1)
struct move_bytes
{
	Byte src;
	Byte dst;
	Byte promote;
	Byte bits;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct moverec
{
	move_bytes b;
} ;
#pragma pack(pop)

struct gen_rec
{
	moverec m;
	int score;
} ;

struct hist_rec
{
	moverec m;
	int capture;
	int castle;
	int ep;
	int fifty;
} ;

class DELPHICLASS TChessThread;
class PASCALIMPLEMENTATION TChessThread : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	int pcsq[2][6][64];
	int flip[64];
	int pawn_pcsq[64];
	int kingside_pawn_pcsq[64];
	int queenside_pawn_pcsq[64];
	int minor_pcsq[64];
	int king_pcsq[64];
	int endgame_king_pcsq[64];
	int color[64];
	int piece[64];
	int side;
	int xside;
	int castle;
	int ep;
	int fifty;
	int ply;
	gen_rec gen_dat[4096];
	int gen_begin[64];
	int gen_end[64];
	int history[64][64];
	hist_rec hist_dat[64];
	int nodes;
	moverec pv[64][64];
	int pv_length[64];
	bool follow_pv;
	int mailbox[120];
	int mailbox64[64];
	bool slide[6];
	int offsets[6];
	int offset[6][8];
	int castle_mask[64];
	int value[6];
	char piece_char[6];
	int init_color[64];
	int init_piece[64];
	bool Thinking;
	bool *WhiteToMove;
	bool *ComputerPlaysWhite;
	bool *ComputerPlaysBlack;
	bool StopThinkingNow;
	char *Position;
	Square *EnPassant;
	CastleSet *Castling;
	int *SearchDepth;
	Classes::TThreadPriority *ThinkingPriority;
	int __fastcall eval(void);
	bool __fastcall attack(int sq, int s);
	int __fastcall ColorOfPiece(Square sq);
	bool __fastcall in_check(int s);
	bool __fastcall makemove(move_bytes m);
	int __fastcall quiesce(int alpha, int beta);
	int __fastcall search(int alpha, int beta, int depth);
	void __fastcall ThinkAboutAMove(void);
	void __fastcall ThinkingFinished(void);
	void __fastcall gen(void);
	void __fastcall gen_caps(void);
	void __fastcall gen_promote(int src, int dst, int bits);
	void __fastcall gen_push(int src, int dst, int bits);
	void __fastcall InitValues(void);
	void __fastcall init_eval(void);
	void __fastcall IntCopy(PINT dest, PINT source, int count);
	void __fastcall PerformMove(void);
	void __fastcall sort(int src);
	void __fastcall sort_pv(void);
	void __fastcall takeback(void);
	
protected:
	virtual void __fastcall Execute(void);
	
public:
	TMoveFunc MoveFunc;
	Classes::TNotifyEvent EndFunc;
	__fastcall TChessThread(void);
public:
	/* TThread.Destroy */ __fastcall virtual ~TChessThread(void) { }
	
};

typedef MoveInfo ChessBrd__4[257][3];

class DELPHICLASS TChessBrd;
class PASCALIMPLEMENTATION TChessBrd : public Controls::TGraphicControl 
{
	typedef Controls::TGraphicControl inherited;
	
private:
	Extctrls::TTimer* timer;
	MoveInfo temp;
	Controls::TCursor OldCursor;
	TChessThread* Now;
	bool GameEnded;
	bool FirstTime;
	MoveInfo MoveList[257][3];
	char buf[261];
	char PromoteTo;
	int PieceIndex[3][7];
	int Boardx;
	int Boardy;
	int PieceSize;
	int _SizeOfSquare;
	int _CurrentMove;
	bool ResizeState;
	bool _resizable;
	int _ResizeMinSize;
	int _ResizeMaxSize;
	bool _ComputerPlaysWhite;
	bool _ComputerPlaysBlack;
	int _SearchDepth;
	TThreadPriority _ThinkingPriority;
	TMoveEvent _legalMove;
	TMoveEvent _check;
	TMoveEvent _mate;
	TMoveEvent _staleMate;
	TMoveEvent _castle;
	TMoveEvent _failed;
	Classes::TNotifyEvent _paint;
	Classes::TNotifyEvent _draw;
	Classes::TNotifyEvent _noMatingMaterial;
	Classes::TNotifyEvent _threefoldPosition;
	TThinkEvent _calculate;
	TCaptureEvent _capture;
	TOneSquareEvent _illegalMove;
	TPromotionEvent _promotion;
	Square _enPassant;
	char _position[66];
	Controls::TImageList* list;
	Graphics::TBitmap* _squareLight;
	Graphics::TBitmap* _squareDark;
	Graphics::TBitmap* _borderBitmap;
	Graphics::TBitmap* _custompieceset;
	Graphics::TBitmap* Default;
	Graphics::TPen* _lineStyle;
	Graphics::TFont* _coordFont;
	CastleSet _castlingAllowed;
	CoordSet _displayCoords;
	bool _customEngine;
	Square SquareClick1;
	Square SquareClick2;
	int _SizeOfBorder;
	int _animationDelay;
	bool _whiteOnTop;
	bool _whiteToMove;
	bool _boardlines;
	bool _animateMoves;
	Graphics::TColor _squareColorLight;
	Graphics::TColor _squareColorDark;
	Graphics::TColor _bordercolor;
	System::AnsiString _version;
	void __fastcall TimerCallback(System::TObject* Sender);
	bool __fastcall CheckLegalBishopMove(Square oldsq, Square newsq);
	bool __fastcall CheckLegalKingMove(Square oldsq, Square newsq);
	bool __fastcall CheckLegalKnightMove(Square oldsq, Square newsq);
	bool __fastcall CheckLegalPawnMove(Square oldsq, Square newsq);
	bool __fastcall CheckLegalRookMove(Square oldsq, Square newsq);
	bool __fastcall CheckLegalQueenMove(Square oldsq, Square newsq);
	bool __fastcall BitmapExists(Graphics::TBitmap* bmp);
	bool __fastcall BitmapIsValidPieceSet(Graphics::TBitmap* bmp);
	bool __fastcall CheckForThreefoldPosition(void);
	int __fastcall PieceToInt(char piece);
	void __fastcall DoPromotion(Square sq);
	void __fastcall ThinkingComplete(System::TObject* Sender);
	void __fastcall DrawBorder(void);
	void __fastcall DrawBoard(void);
	void __fastcall DrawBoardLines(void);
	void __fastcall DrawPieces(void);
	void __fastcall DrawPiece(Square sq, char piece);
	void __fastcall InitializeBitmap(void);
	void __fastcall OrganizeBitmaps(void);
	void __fastcall AnimateHorizontally(int x1, int x2, int y, int delay);
	void __fastcall AnimateVertically(int y1, int y2, int x, int delay);
	void __fastcall AnimateDiagonally(int x1, int y1, int x2, int y2, int delay);
	void __fastcall SetNewGame(void);
	System::AnsiString __fastcall Get_Position();
	bool __fastcall Get_Thinking(void);
	void __fastcall Set_BoardLines(bool show);
	void __fastcall Set_BorderBitmap(Graphics::TBitmap* bmp);
	void __fastcall Set_BorderColor(Graphics::TColor c);
	void __fastcall Set_ComputerPlaysBlack(bool plays);
	void __fastcall Set_ComputerPlaysWhite(bool plays);
	void __fastcall Set_CoordFont(Graphics::TFont* f);
	void __fastcall Set_CurrentMove(int moveno);
	void __fastcall Set_CustomPieceSet(Graphics::TBitmap* bmp);
	void __fastcall Set_CustomEngine(bool use);
	void __fastcall Set_DarkSquare(Graphics::TBitmap* bmp);
	void __fastcall Set_DisplayCoords(CoordSet cset);
	void __fastcall Set_EnPassant(Square sq);
	void __fastcall Set_LightSquare(Graphics::TBitmap* bmp);
	void __fastcall Set_LineStyle(Graphics::TPen* pen);
	void __fastcall Set_Position(System::AnsiString pos);
	void __fastcall Set_ResizeMaxSize(int size);
	void __fastcall Set_ResizeMinSize(int size);
	void __fastcall Set_SearchDepth(int depth);
	void __fastcall Set_SizeOfBorder(int border);
	void __fastcall Set_SizeOfSquare(int size);
	void __fastcall Set_SquareColorDark(Graphics::TColor c);
	void __fastcall Set_SquareColorLight(Graphics::TColor c);
	void __fastcall Set_Thinking(bool thinking);
	void __fastcall Set_ThinkingPriority(Classes::TThreadPriority priority);
	void __fastcall Set_Version(System::AnsiString str);
	void __fastcall Set_WhiteOnTop(bool wabove);
	void __fastcall Set_WhiteToMove(bool wmove);
	
protected:
	DYNAMIC void __fastcall Click(void);
	DYNAMIC void __fastcall DragCanceled(void);
	DYNAMIC void __fastcall DragDrop(System::TObject* Source, int X, int Y);
	DYNAMIC void __fastcall DragOver(System::TObject* Source, int X, int Y, Controls::TDragState State, 
		bool &Accept);
	HIDESBASE void __fastcall EndDrag(bool drop);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int 
		Y);
	virtual void __fastcall Paint(void);
	void __fastcall Promotion(System::TObject* Sender, Square oldSq, Square newSq, char &NewPiece);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	
public:
	int FirstMove;
	int LastMove;
	bool FirstTurn;
	bool LastTurn;
	__fastcall virtual TChessBrd(Classes::TComponent* AOwner);
	__fastcall virtual ~TChessBrd(void);
	bool __fastcall BlackInCheckAfter(Square oldsq, Square newsq);
	int __fastcall ColorOfPiece(char piece);
	int __fastcall ColorOfPieceOnSquare(Square sq);
	int __fastcall ColorOfSquare(Square sq);
	MoveInfo __fastcall GetMove(int moveno, bool whiteMoves);
	bool __fastcall GotoMove(int moveno, bool whiteMoves);
	bool __fastcall LegalMoveAvailable(void);
	Square __fastcall MouseToSquare(int x, int y);
	bool __fastcall Move(Square oldsq, Square newsq);
	bool __fastcall MoveBackward(void);
	bool __fastcall MoveForward(void);
	bool __fastcall MoveIsLegal(Square oldsq, Square newsq);
	bool __fastcall PerformMove(Square oldsq, Square newsq);
	bool __fastcall SetUpPosition(const MoveInfo &pos, int moveno, bool whiteMoves);
	Square __fastcall StringToSquare(System::AnsiString str);
	bool __fastcall WhiteInCheckAfter(Square oldsq, Square newsq);
	Square __fastcall WindowToSquare(int x, int y);
	int __fastcall XPos(Square sq);
	int __fastcall YPos(Square sq);
	void __fastcall Animate(Square oldsq, Square newsq, int delay);
	void __fastcall CancelThinking(void);
	void __fastcall ClearSquare(Square sq);
	void __fastcall DrawChessPiece(Graphics::TCanvas* canvas, int x, int y, char piece);
	void __fastcall GetMoveList(Classes::TStringList* &list);
	void __fastcall NewGame(void);
	void __fastcall SquareToCoords(Square sq, int &x, int &y);
	void __fastcall Think(void);
	void __fastcall UpdateChessBoard(System::AnsiString oldpos);
	
__published:
	__property bool AnimateMoves = {read=_animateMoves, write=_animateMoves, nodefault};
	__property int AnimationDelay = {read=_animationDelay, write=_animationDelay, nodefault};
	__property bool BoardLines = {read=_boardlines, write=Set_BoardLines, nodefault};
	__property Graphics::TBitmap* BorderBitmap = {read=_borderBitmap, write=Set_BorderBitmap};
	__property Graphics::TColor BorderColor = {read=_bordercolor, write=Set_BorderColor, nodefault};
	__property CastleSet CastlingAllowed = {read=_castlingAllowed, write=_castlingAllowed, nodefault};
	__property bool ComputerPlaysBlack = {read=_ComputerPlaysBlack, write=Set_ComputerPlaysBlack, nodefault
		};
	__property bool ComputerPlaysWhite = {read=_ComputerPlaysWhite, write=Set_ComputerPlaysWhite, nodefault
		};
	__property bool Thinking = {read=Get_Thinking, write=Set_Thinking, nodefault};
	__property Graphics::TFont* CoordFont = {read=_coordFont, write=Set_CoordFont};
	__property int CurrentMove = {read=_CurrentMove, write=Set_CurrentMove, nodefault};
	__property Graphics::TBitmap* CustomPieceSet = {read=_custompieceset, write=Set_CustomPieceSet};
	__property CoordSet DisplayCoords = {read=_displayCoords, write=Set_DisplayCoords, nodefault};
	__property bool CustomEngine = {read=_customEngine, write=Set_CustomEngine, nodefault};
	__property Square EnPassant = {read=_enPassant, write=Set_EnPassant, nodefault};
	__property Graphics::TPen* LineStyle = {read=_lineStyle, write=Set_LineStyle};
	__property System::AnsiString Position = {read=Get_Position, write=Set_Position};
	__property bool Resizable = {read=_resizable, write=_resizable, nodefault};
	__property int ResizeMinSize = {read=_ResizeMinSize, write=Set_ResizeMinSize, nodefault};
	__property int ResizeMaxSize = {read=_ResizeMaxSize, write=Set_ResizeMaxSize, nodefault};
	__property int SearchDepth = {read=_SearchDepth, write=Set_SearchDepth, nodefault};
	__property int SizeOfBorder = {read=_SizeOfBorder, write=Set_SizeOfBorder, nodefault};
	__property int SizeOfSquare = {read=_SizeOfSquare, write=Set_SizeOfSquare, nodefault};
	__property Graphics::TColor SquareColorDark = {read=_squareColorDark, write=Set_SquareColorDark, nodefault
		};
	__property Graphics::TColor SquareColorLight = {read=_squareColorLight, write=Set_SquareColorLight, 
		nodefault};
	__property Graphics::TBitmap* SquareDark = {read=_squareDark, write=Set_DarkSquare};
	__property Graphics::TBitmap* SquareLight = {read=_squareLight, write=Set_LightSquare};
	__property bool WhiteOnTop = {read=_whiteOnTop, write=Set_WhiteOnTop, nodefault};
	__property bool WhiteToMove = {read=_whiteToMove, write=Set_WhiteToMove, nodefault};
	__property Classes::TThreadPriority ThinkingPriority = {read=_ThinkingPriority, write=Set_ThinkingPriority
		, nodefault};
	__property System::AnsiString Version = {read=_version, write=Set_Version};
	__property DragCursor ;
	__property DragMode ;
	__property Enabled ;
	__property Visible ;
	__property TCaptureEvent OnCapture = {read=_capture, write=_capture};
	__property TMoveEvent OnCastle = {read=_castle, write=_castle};
	__property TMoveEvent OnCheck = {read=_check, write=_check};
	__property Classes::TNotifyEvent OnDraw = {read=_draw, write=_draw};
	__property TOneSquareEvent OnIllegalMove = {read=_illegalMove, write=_illegalMove};
	__property TMoveEvent OnLegalMove = {read=_legalMove, write=_legalMove};
	__property TMoveEvent OnMate = {read=_mate, write=_mate};
	__property Classes::TNotifyEvent OnNoMatingMaterial = {read=_noMatingMaterial, write=_noMatingMaterial
		};
	__property Classes::TNotifyEvent OnPaint = {read=_paint, write=_paint};
	__property TPromotionEvent OnPromotion = {read=_promotion, write=_promotion};
	__property TMoveEvent OnStaleMate = {read=_staleMate, write=_staleMate};
	__property TThinkEvent OnCalculateMove = {read=_calculate, write=_calculate};
	__property TMoveEvent OnCalculationFailed = {read=_failed, write=_failed};
	__property Classes::TNotifyEvent OnThreefoldPosition = {read=_threefoldPosition, write=_threefoldPosition
		};
	__property OnClick ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDrag ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDrag ;
};

class DELPHICLASS ChessBrdError;
class PASCALIMPLEMENTATION ChessBrdError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ __fastcall ChessBrdError(const System::AnsiString Msg) : Sysutils::Exception(
		Msg) { }
	/* Exception.CreateFmt */ __fastcall ChessBrdError(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall ChessBrdError(int Ident, Extended Dummy) : Sysutils::Exception(
		Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall ChessBrdError(int Ident, const System::TVarRec * Args, const 
		int Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall ChessBrdError(const System::AnsiString Msg, int AHelpContext)
		 : Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall ChessBrdError(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall ChessBrdError(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall ChessBrdError(int Ident, const System::TVarRec * Args, 
		const int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext)
		 { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~ChessBrdError(void) { }
	
};

//-- var, const, procedure ---------------------------------------------------
#define SetAndrew40Str "SETANDREW40"
#define versionStr "3.02"
#define NoPiece (Shortint)(-1)
#define Black (Byte)(0)
#define White (Byte)(1)
#define MOVE_STACK (Word)(4096)
#define HIST_STACK (Byte)(64)
#define LIGHT (Byte)(0)
#define DARK (Byte)(1)
#define PAWN (Byte)(0)
#define KNIGHT (Byte)(1)
#define BISHOP (Byte)(2)
#define ROOK (Byte)(3)
#define QUEEN (Byte)(4)
#define KING (Byte)(5)
#define EMPTY (Byte)(6)
extern PACKAGE void __fastcall Register(void);

}	/* namespace Chessbrd */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Chessbrd;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// ChessBrd
