namespace Aether.Textures

type ConstantTexture<'T>(value : 'T) =
    inherit Texture<'T>()

    override this.Evaluate(dg) = value