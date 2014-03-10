namespace Aether.Textures

open Aether.Math
open Aether.Geometry
open Aether.Transforms
open Aether.Shapes


type MappedTexture2D = { S : single; T : single;
                         DsDx : single; DtDx : single;
                         DsDy : single; DtDy : single }

[<AbstractClass>]
type TextureMapping2D() =
    abstract member Map : DifferentialGeometry -> MappedTexture2D


type UVMapping2D(su, sv, du, dv) =
    inherit TextureMapping2D()

    override this.Map dg =
        { S = su * dg.U + du;
          T = sv * dg.V + dv;
          
          // Compute texture differentials for 2D identity mapping.
          DsDx = su * dg.DuDx;
          DtDx = sv * dg.DvDx;
          DsDy = su * dg.DuDy;
          DtDy = sv * dg.DvDy }


type SphericalMapping2D(worldToTexture : Transform) =
    inherit TextureMapping2D()

    let sphere (p : Point) =
        let vector = Vector.Normalize(worldToTexture.Transform(p) - Point.Zero)
        let theta = Vector.SphericalTheta(vector)
        let phi = Vector.SphericalPhi(vector)
        (theta * invPi, phi * invTwoPi)

    override this.Map dg =
        let s, t = sphere dg.Point

        // Compute texture coordinate differentials for sphere (u,v) mapping.
        let delta = 0.1f

        let sx, tx = sphere (dg.Point + delta * dg.DpDx)
        let dsdx = (sx - s) / delta
        let dtdx' = (tx - t) / delta
        let dtdx = if dtdx' > 0.5f then 1.0f - dtdx'
                   else if dtdx' < -0.5f then -(dtdx' + 1.0f)
                   else dtdx'

        let sy, ty = sphere (dg.Point + delta * dg.DpDy)
        let dsdy = (sy - s) / delta
        let dtdy' = (ty - t) / delta
        let dtdy = if dtdy' > 0.5f then 1.0f - dtdy'
                   else if dtdy' < -0.5f then -(dtdy' + 1.0f)
                   else dtdy'

        { S = s;
          T = t;
          DsDx = dsdx;
          DtDx = dtdx;
          DsDy = dsdy;
          DtDy = dtdy }


type CylindricalMapping2D(worldToTexture : Transform) =
    inherit TextureMapping2D()

    let cylinder (p : Point) =
        let vector = Vector.Normalize(worldToTexture.Transform(p) - Point.Zero)
        let s = (pi + (atan2 vector.Y vector.X)) / (2.0f * pi)
        let t = vector.Z
        (s, t)

    override this.Map dg =
        let s, t = cylinder dg.Point

        // Compute texture coordinate differentials for cylinder (u,v) mapping.
        let delta = 0.01f

        let sx, tx = cylinder (dg.Point + delta * dg.DpDx)
        let dsdx = (sx - s) / delta
        let dtdx' = (tx - t) / delta
        let dtdx = if dtdx' > 0.5f then 1.0f - dtdx'
                   else if dtdx' < -0.5f then -(dtdx' + 1.0f)
                   else dtdx'

        let sy, ty = cylinder (dg.Point + delta * dg.DpDy)
        let dsdy = (sy - s) / delta
        let dtdy' = (ty - t) / delta
        let dtdy = if dtdy' > 0.5f then 1.0f - dtdy'
                   else if dtdy' < -0.5f then -(dtdy' + 1.0f)
                   else dtdy'

        { S = s;
          T = t;
          DsDx = dsdx;
          DtDx = dtdx;
          DsDy = dsdy;
          DtDy = dtdy }


type PlanarMapping2D(vs : Vector, vt : Vector, ds, dt) =
    inherit TextureMapping2D()

    override this.Map dg =
        let vector = dg.Point - Point.Zero
        { S = ds + Vector.Dot(vector, vs);
          T = dt + Vector.Dot(vector, vt);
          DsDx = Vector.Dot(dg.DpDx, vs);
          DtDx = Vector.Dot(dg.DpDx, vt);
          DsDy = Vector.Dot(dg.DpDy, vs);
          DtDy = Vector.Dot(dg.DpDy, vt) }


type MappedTexture3D = { Point : Point;
                         DpDx : Vector; 
                         DpDy : Vector }


[<AbstractClass>]
type TextureMapping3D() =
    abstract member Map : DifferentialGeometry -> MappedTexture3D


type IdentityMapping3D(worldToTexture : Transform) =
    inherit TextureMapping3D()

    override this.Map dg =
        { Point = worldToTexture.Transform(dg.Point);
          DpDx = worldToTexture.Transform(dg.DpDx);
          DpDy = worldToTexture.Transform(dg.DpDy) }


[<AbstractClass>]
type Texture<'T>() =
    abstract member Evaluate : DifferentialGeometry -> 'T


type ConstantTexture<'T>(value : 'T) =
    inherit Texture<'T>()

    override this.Evaluate(dg) = value