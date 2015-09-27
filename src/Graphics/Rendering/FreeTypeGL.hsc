{-# Language DeriveDataTypeable #-}

module Graphics.Rendering.FreeTypeGL
    ( initContext
    , Font
    , Vec2(..)
    , Vec4(..)
    , FontManager
    , fromMarkup
    , TextBuffer
    , new
    , delete
    , render
    , addText
    , Markup(..)
    ) where

import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.Raw.Types

foreign import ccall "GL/glew.h &glewExperimental" glewExperimental :: Ptr GLboolean
foreign import ccall "GL/glew.h glewInit" glewInit :: IO GLenum

data GLEWException = GLEWException GLenum deriving (Eq, Ord, Typeable, Show)
instance Exception GLEWException

-- | Initialise the GL context for loading textures.
-- currently this just initialises /glew/.
initContext :: IO ()
initContext = do
    poke glewExperimental 1
    e <- glewInit
    case e of
        0 -> return ()
        _ -> throwIO (GLEWException e)

data Font

#include <vec234.h>

data Vec2 = Vec2 CFloat CFloat deriving (Show, Read, Eq, Ord)
data Vec4 = Vec4 CFloat CFloat CFloat CFloat deriving (Show, Read, Eq, Ord)

instance Storable Vec2 where
    sizeOf _ = (#size vec2)
    alignment _ = (#const __alignof__(vec2))
    peek ptr = do
        m <- return Vec2
            `ap` (#peek vec2, x) ptr
            `ap` (#peek vec2, y) ptr
        return $! m
    poke ptr (Vec2 x y) = do
        (#poke vec2, x) ptr x
        (#poke vec2, y) ptr y

instance Storable Vec4 where
    sizeOf _ = (#size vec4)
    alignment _ = (#const __alignof__(vec4))
    peek ptr = do
        m <- return Vec4
            `ap` (#peek vec4, x) ptr
            `ap` (#peek vec4, y) ptr
            `ap` (#peek vec4, z) ptr
            `ap` (#peek vec4, w) ptr
        return $! m
    poke ptr (Vec4 x y z w) = do
        (#poke vec4, x) ptr x
        (#poke vec4, y) ptr y
        (#poke vec4, z) ptr z
        (#poke vec4, w) ptr w

data FontManager

foreign import ccall "font-manager.h font_manager_get_from_markup" fromMarkup :: Ptr FontManager -> Ptr Markup -> Ptr Font

data VertexBuffer

data TextBuffer = TextBuffer
    { buffer :: Ptr VertexBuffer
    , manager :: Ptr FontManager
    , baseColor :: Vec4
    , origin :: Vec2
    , lineStart :: CSize
    , lineAscender :: CFloat
    , lineDescender :: CFloat
    , shader :: GLuint
    , shaderTexture :: GLuint
    , shaderPixel :: GLuint
    } deriving (Show)

#include <text-buffer.h>

instance Storable TextBuffer where
    sizeOf _ = (#size text_buffer_t)
    alignment _ = (#const __alignof__(text_buffer_t))
    peek ptr = do
        m <- return TextBuffer
            `ap` (#peek text_buffer_t, buffer) ptr
            `ap` (#peek text_buffer_t, manager) ptr
            `ap` (#peek text_buffer_t, base_color) ptr
            `ap` (#peek text_buffer_t, origin) ptr
            `ap` (#peek text_buffer_t, line_start) ptr
            `ap` (#peek text_buffer_t, line_ascender) ptr
            `ap` (#peek text_buffer_t, line_descender) ptr
            `ap` (#peek text_buffer_t, shader) ptr
            `ap` (#peek text_buffer_t, shader_texture) ptr
            `ap` (#peek text_buffer_t, shader_pixel) ptr
        return $! m
    poke ptr obj = do
        (#poke text_buffer_t, buffer) ptr (buffer obj)
        (#poke text_buffer_t, manager) ptr (manager obj)
        (#poke text_buffer_t, base_color) ptr (baseColor obj)
        (#poke text_buffer_t, origin) ptr (origin obj)
        (#poke text_buffer_t, line_start) ptr (lineStart obj)
        (#poke text_buffer_t, line_ascender) ptr (lineAscender obj)
        (#poke text_buffer_t, line_descender) ptr (lineDescender obj)
        (#poke text_buffer_t, shader) ptr (shader obj)
        (#poke text_buffer_t, shader_texture) ptr (shaderTexture obj)
        (#poke text_buffer_t, shader_pixel) ptr (shaderPixel obj)

foreign import ccall "text-buffer.h text_buffer_new" new :: CSize -> IO (Ptr TextBuffer)
foreign import ccall "text-buffer.h text_buffer_delete" delete :: Ptr TextBuffer -> IO ()
foreign import ccall "text-buffer.h text_buffer_render" render :: Ptr TextBuffer -> IO ()
foreign import ccall "text-buffer.h text_buffer_add_text" addText :: Ptr TextBuffer -> Ptr Vec2 -> Ptr Markup -> Ptr CWchar -> CSize -> IO ()

data Markup = Markup
    { family :: Ptr CChar
    , size :: CFloat
    , bold :: CInt
    , italic :: CInt
    , rise :: CFloat
    , spacing :: CFloat
    , gamma :: CFloat
    , foregroundColor :: Vec4
    , backgroundColor :: Vec4
    , outline :: CInt
    , outlineColor :: Vec4
    , underline :: CInt
    , underlineColor :: Vec4
    , overline :: CInt
    , overlineColor :: Vec4
    , strikethrough :: CInt
    , strikethroughColor :: Vec4
    , font :: Ptr Font
    } deriving (Show)

#include <markup.h>

instance Storable Markup where
    sizeOf _ = (#size markup_t)
    alignment _ = (#const __alignof__(markup_t))
    peek ptr = do
        m <- return Markup
            `ap` (#peek markup_t, family) ptr
            `ap` (#peek markup_t, size) ptr
            `ap` (#peek markup_t, bold) ptr
            `ap` (#peek markup_t, italic) ptr
            `ap` (#peek markup_t, rise) ptr
            `ap` (#peek markup_t, spacing) ptr
            `ap` (#peek markup_t, gamma) ptr
            `ap` (#peek markup_t, foreground_color) ptr
            `ap` (#peek markup_t, background_color) ptr
            `ap` (#peek markup_t, outline) ptr
            `ap` (#peek markup_t, outline_color) ptr
            `ap` (#peek markup_t, underline) ptr
            `ap` (#peek markup_t, underline_color) ptr
            `ap` (#peek markup_t, overline) ptr
            `ap` (#peek markup_t, overline_color) ptr
            `ap` (#peek markup_t, strikethrough) ptr
            `ap` (#peek markup_t, strikethrough_color) ptr
            `ap` (#peek markup_t, font) ptr
        return $! m
    poke ptr obj = do
        (#poke markup_t, family) ptr (family obj)
        (#poke markup_t, size) ptr (size obj)
        (#poke markup_t, bold) ptr (bold obj)
        (#poke markup_t, italic) ptr (italic obj)
        (#poke markup_t, rise) ptr (rise obj)
        (#poke markup_t, spacing) ptr (spacing obj)
        (#poke markup_t, gamma) ptr (gamma obj)
        (#poke markup_t, foreground_color) ptr (foregroundColor obj)
        (#poke markup_t, background_color) ptr (backgroundColor obj)
        (#poke markup_t, outline) ptr (outline obj)
        (#poke markup_t, outline_color) ptr (outlineColor obj)
        (#poke markup_t, underline) ptr (underline obj)
        (#poke markup_t, underline_color) ptr (underlineColor obj)
        (#poke markup_t, overline) ptr (overline obj)
        (#poke markup_t, overline_color) ptr (overlineColor obj)
        (#poke markup_t, strikethrough) ptr (strikethrough obj)
        (#poke markup_t, strikethrough_color) ptr (strikethroughColor obj)
        (#poke markup_t, font) ptr (font obj)
