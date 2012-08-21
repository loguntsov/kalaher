-module(kalah).

-export([start/0, stop/0]).

-include_lib("wx/include/wx.hrl").

-define(ABOUT, ?wxID_ABOUT).
-define(EXIT, ?wxID_EXIT).

start() ->
	WX = wx:new(),
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Kalah"),
	setup(WX, Frame),
	wxFrame:show(Frame),
	loop(Frame),
	wx:destroy().

stop() ->
	wx:destroy().


setup(Wx, Frame) -> ok

	.

loop(Frame) ->
	receive
		_ -> loop(Frame)
	end
.
