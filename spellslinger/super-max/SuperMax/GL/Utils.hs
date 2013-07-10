module SuperMax.GL.Utils (
        -- * Basic
        initRendering, checkError,

        -- *
        makeShaderProgram,
        loadTexture,

        -- * Fonts
        GLFont, loadFontFromImage, writeText2D
    ) where

import Codec.Picture ( DynamicImage(..), readImage )
import Control.Monad ( unless )
import Data.Char ( ord )
import Data.Vect.Float ( Mat4 )
import Data.Vector.Storable ( unsafeWith )
import Foreign.C.String ( withCString )
import Foreign.C.Types ( CFloat(..) )
import Foreign.Marshal.Safe ( with, withArrayLen )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.Program ( Program(..) )
import qualified Codec.Picture as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Graphics.Rendering.OpenGL.Raw as Raw
import System.IO ( hPutStrLn, stderr )
import Text.Printf ( printf )
import Unsafe.Coerce ( unsafeCoerce )

-- | Basic OpenGL initialization.
initRendering :: IO ()
initRendering = do
    [vao] <- genObjectNames 1
    bindVertexArrayObject $= Just vao

-- FIXME Support fall-back shaders.

-- | Create a shader program using vertex and fragment shaders from the given files.
makeShaderProgram :: FilePath       -- ^ Vertex shader path
                  -> FilePath       -- ^ Fragment shader path
                  -> IO Program
makeShaderProgram vertexShaderPath fragmentShaderPath = do
    -- Compile shaders
    vertexShader <- compileShaderSource vertexShaderPath
    fragmentShader <- compileShaderSource fragmentShaderPath

    -- Link and set shader program
    program_ <- createProgram [vertexShader] [fragmentShader]

    checkError "link"

    return program_
  where
    -- | Read a shader from the given file path and compile it into a shader.
    compileShaderSource :: Shader s => String -> IO s
    compileShaderSource path = do
        [shader] <- genObjectNames 1
        text <- BS.readFile path
        shaderSource shader $= [BL.unpack (BL.fromStrict text)]
        compileAndCheck shader
        checkError ("shader " ++ path)
        return shader

    -- | Compile and check a shader.
    compileAndCheck :: Shader s => s -> IO ()
    compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

    -- | Helper that runs an action, checks if it was successful, and prints any warnings.
    checked :: (t -> IO ())
            -> (t -> GettableStateVar Bool)
            -> (t -> GettableStateVar String)
            -> String
            -> t
            -> IO ()
    checked action getStatus getInfoLog message object = do
        action object
        status <- get (getStatus object)
        unless status $
            hPutStrLn stderr . ((message ++ " log: ") ++) =<< get (getInfoLog object)

    -- | Link the given shaders into a shader program.
    createProgram :: [VertexShader] -> [FragmentShader] -> IO Program
    createProgram vertexShaders fragmentShaders = do
        [program_] <- genObjectNames 1
        attachedShaders program_ $= (vertexShaders, fragmentShaders)
        linkAndCheck program_
        return program_

    -- | Link and check a shader program.
    linkAndCheck :: Program -> IO ()
    linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

-- | Check for and print OpenGL errors.
checkError :: String -> IO ()
checkError tag = get errors >>= mapM_ reportError
   where
     reportError err = hPutStrLn stderr (showError err)
     showError (Error category message) =
         printf "GL error %s (%s) detected in %s" (show category) message tag

-- | Load the image as a shader.
loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    eImage <- readImage path

    [tex] <- genObjectNames 1
    textureBinding Texture2D $= Just tex

    case eImage of
        Left err -> do
            error err
        Right (ImageRGBA8 image) -> do
            unsafeWith (P.imageData image) $ \dataPtr ->
                texImage2D Nothing
                           NoProxy
                           0
                           RGBA8
                           (TextureSize2D (fromIntegral (P.imageWidth image))
                                          (fromIntegral (P.imageHeight image)))
                           0
                           (PixelData RGBA UnsignedByte dataPtr)
        Right (ImageRGB8 image) -> do
            unsafeWith (P.imageData image) $ \dataPtr ->
                texImage2D Nothing
                           NoProxy
                           0
                           RGBA8
                           (TextureSize2D (fromIntegral (P.imageWidth image))
                                          (fromIntegral (P.imageHeight image)))
                           0
                           (PixelData RGB UnsignedByte dataPtr)
        Right dynImage -> do
            error (printf "texture '%s' not in 8-bit RBG[A] format (%s found)"
                          path
                          (guessImageFormat dynImage))

    -- FIXME Should we enable mipmaps?  It makes things more blurry.  Also, the second
    -- Linear' should be GL_LINEAR_MIPMAP_LINEAR.

    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

    -- textureFilter Texture2D $= ((Linear', Nothing), Linear')
    -- generateMipmap Texture2D $= Enabled

    checkError "loadTexture"

    return tex
  where
    guessImageFormat :: DynamicImage -> String
    guessImageFormat (ImageY8 _)     = "8-bit greyscale"
    guessImageFormat (ImageY16 _)    = "16-bin greyscale"
    guessImageFormat (ImageYF _)     = "HDR greyscale"
    guessImageFormat (ImageYA8 _)    = "8-bit greyscale with alpha"
    guessImageFormat (ImageYA16 _)   = "16-bit greyscale with alpha"
    guessImageFormat (ImageRGB8 _)   = "8-bit RGB"
    guessImageFormat (ImageRGB16 _)  = "16-bit RGB"
    guessImageFormat (ImageRGBF _)   = "HDR RGB"
    guessImageFormat (ImageRGBA8 _)  = "8-bit RGBA"
    guessImageFormat (ImageRGBA16 _) = "16-bit RGBA"
    guessImageFormat (ImageYCbCr8 _) = "JPEG"
    guessImageFormat (ImageCMYK8 _)  = "8-bit CMYK"
    guessImageFormat (ImageCMYK16 _) = "16-bit CMYK"

data GLFont = GLFont
    { textProgram       :: Program
    , textVertexBuffer  :: BufferObject
    , textUVBuffer      :: BufferObject
    , fontTexture       :: TextureObject
    , samplerId         :: GLint
    , textVertexArrayId :: AttribLocation
    , textUVArrayId     :: AttribLocation
    , mvpId             :: GLint
    }

-- | Load the font from the given image.  The image should be square, its dimensions
-- should be a power of 2, and the letters should be placed in a 16x16 table with no
-- borders in ASCII order.
loadFontFromImage :: FilePath -> IO GLFont
loadFontFromImage path = do
    fontTexture_ <- loadTexture path

    [textVertexBuffer_] <- genObjectNames 1
    [textUVBuffer_] <- genObjectNames 1

    textProgram_ <- makeShaderProgram "textVertex.glsl" "textFragment.glsl"

    samplerId_ <- withCString "textureSampler" $ \idS ->
        Raw.glGetUniformLocation (programID textProgram_) idS

    mvpId_ <- withCString "MVP" $ \idMVP ->
        Raw.glGetUniformLocation (programID textProgram_) idMVP

    textVertexArrayId_ <- get (attribLocation textProgram_ "vertexPosition")
    textUVArrayId_ <- get (attribLocation textProgram_ "uvPosition")

    checkError "loadFont"

    return (GLFont { textProgram       = textProgram_
                   , textVertexBuffer  = textVertexBuffer_
                   , textUVBuffer      = textUVBuffer_
                   , fontTexture       = fontTexture_
                   , samplerId         = samplerId_
                   , textVertexArrayId = textVertexArrayId_
                   , textUVArrayId     = textUVArrayId_
                   , mvpId             = mvpId_
                   })

sizeOfFloat :: Int
sizeOfFloat = sizeOf (undefined :: GLfloat)

-- | Print the given text to the screen at the given position.
writeText2D :: GLFont           -- ^ The font to use
            -> String           -- ^ The text to display
            -> Float            -- ^ x in [-1, +1]
            -> Float            -- ^ y in [-1, +1]
            -> Float            -- ^ Size
            -> Mat4             -- ^ MVP
            -> IO ()
writeText2D font text x y size mvp = do
    -- Compute vertices and UVs
    let vertices = map CFloat $ flip concatMap [0..length text - 1] $ \i' ->
            let i = fromIntegral i' in
            [ x + i * size, y + size
            , x + i * size, y
            , x + i * size + size, y + size
            , x + i * size + size, y
            , x + i * size + size, y + size
            , x + i * size, y ]
        uvs = map CFloat $ flip concatMap text $ \ch ->
            let uv_x = fromIntegral (ord ch `mod` 16) / 16.0
                uv_y = fromIntegral (ord ch `div` 16) / 16.0 :: Float
            in [ uv_x, uv_y
               , uv_x, uv_y + 1.0/16.0
               , uv_x + 1.0/16.0, uv_y
               , uv_x + 1.0/16.0, uv_y + 1.0/16.0
               , uv_x + 1.0/16.0, uv_y
               , uv_x, uv_y + 1.0/16.0 ]

    -- Send vertices and UVs to OpenGL
    bindBuffer ArrayBuffer $= Just (textVertexBuffer font)
    withArrayLen vertices $ \len verticesData -> do
        bufferData ArrayBuffer $= ( fromIntegral (len * sizeOfFloat)
                                  , verticesData
                                  , StaticDraw )

    bindBuffer ArrayBuffer $= Just (textUVBuffer font)
    withArrayLen uvs $ \len uvsData -> do
        bufferData ArrayBuffer $= ( fromIntegral (len * sizeOfFloat)
                                  , uvsData
                                  , StaticDraw )

    -- Draw text

    currentProgram $= Just (textProgram font)

    -- TODO Submit a bug because the 'Uniform TextureUnit' instance doesn't work and
    -- causes INVALID_OPERATION errors.

    Raw.glUniform1i (samplerId font) 0
    activeTexture $= (TextureUnit 0)
    textureBinding Texture2D $= Just (fontTexture font)

    with mvp $ \mvpPtr -> Raw.glUniformMatrix4fv (mvpId font) 1 0 (unsafeCoerce mvpPtr)

    vertexAttribArray (textVertexArrayId font) $= Enabled
    vertexAttribArray (textUVArrayId font) $= Enabled

    bindBuffer ArrayBuffer $= Just (textVertexBuffer font)
    vertexAttribPointer (textVertexArrayId font) $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)

    bindBuffer ArrayBuffer $= Just (textUVBuffer font)
    vertexAttribPointer (textUVArrayId font) $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)

    drawArrays Triangles 0 (fromIntegral (length vertices))

    vertexAttribArray (textUVArrayId font) $= Disabled
    vertexAttribArray (textVertexArrayId font) $= Disabled

    checkError "writeText2D"
