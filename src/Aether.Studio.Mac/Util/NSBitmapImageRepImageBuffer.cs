using Nexus.Graphics;
using Nexus;
using MonoMac.AppKit;

namespace Aether.Studio.Mac
{
    public class NSBitmapImageRepImageBuffer : IImageBuffer
    {
        private readonly NSBitmapImageRep _imageRep;

        public ColorF this [int x, int y]
        {
            get { return ToNexusColor(_imageRep.ColorAt(x, y)); }
            set { _imageRep.SetColorAt(ToNSColor(value), x, y); }
        }

        public NSBitmapImageRepImageBuffer(NSBitmapImageRep imageRep)
        {
            _imageRep = imageRep;
        }

        private static ColorF ToNexusColor(NSColor c)
        {
            return new ColorF(c.AlphaComponent, c.RedComponent, c.GreenComponent, c.BlueComponent);
        }

        private static NSColor ToNSColor(ColorF c)
        {
            return NSColor.FromDeviceRgba(c.R, c.G, c.B, c.A);
        }
    }
}

