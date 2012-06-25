using System;

namespace Aether.Materials.Reflectance
{
	[Flags]
	public enum BxdfType
	{
		Reflection = 0,
        Transmission = 1,

		Diffuse    = 2,
        Glossy = 3,
        Specular = 4,

        AllTypes = Diffuse | Glossy | Specular,

        AllReflection = Reflection | AllTypes,
        AllTransmission = Transmission | AllTypes,

        All = AllReflection | AllTransmission
	}
}