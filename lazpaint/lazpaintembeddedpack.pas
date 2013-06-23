{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazPaintEmbeddedPack; 

interface

uses
    LazpaintInstance, LazpaintMainForm, LazPaintType, uabout, ucanvassize, 
  uchoosecolor, uclipboard, ucolorintensity, ucolorize, ucommandline, uconfig, 
  ucursors, ucustomblur, uemboss, ugraph, uimage, umac, umotionblur, 
  umultiimage, unewimage, uparse, uradialblur, uresample, ushiftcolors, utool, 
  utoolbox, utwirl, uimagestate, upixelate, uresourcestrings, uscaledpi, 
  ustatehandler, utoolbasic, utooldeformationgrid, utoolfloodfill, 
  utoolpolygon, utoolselect, utooltext, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('LazPaintEmbeddedPack', @Register); 
end.
