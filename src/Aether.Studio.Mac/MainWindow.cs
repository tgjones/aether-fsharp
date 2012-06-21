
using System;
using System.Collections.Generic;
using System.Linq;
using MonoMac.Foundation;
using MonoMac.AppKit;
using Aether.Studio.Shared;
using Aether.Films;
using System.Runtime.InteropServices;
using System.Drawing;
using Nexus;
using MonoMac.CoreGraphics;

namespace Aether.Studio.Mac
{
    public partial class MainWindow : MonoMac.AppKit.NSWindow
    {
        const int SAMPLE_SIZE = 8;
        const int SAMPLE_NUMBER = 3;
        const int PIXEL_BYTES = 3;

		#region Constructors
		
        // Called when created from unmanaged code
        public MainWindow(IntPtr handle) : base (handle)
        {
            Initialize();
        }
		
        // Called when created directly from a XIB file
        [Export ("initWithCoder:")]
        public MainWindow(NSCoder coder) : base (coder)
        {
            Initialize();
        }
		
        // Shared initialization code
        void Initialize()
        {

        }
		
		#endregion

        partial void OnRenderClick(NSObject sender)
        {
            Render();
        }

        void Render()
        {
            var size = ImageView.Bounds.Size;
            int pixelWidth = (int) size.Width, pixelHeight = (int) size.Height;
            //var bitmapData = new char[pixelWidth * pixelHeight * PIXEL_BYTES];

            // TODO: Clean up this memory
            var bitmapData = Marshal.AllocHGlobal(pixelWidth * pixelHeight * PIXEL_BYTES);
            var imageRep = new NSBitmapImageRep(bitmapData, pixelWidth, pixelHeight, SAMPLE_SIZE, SAMPLE_NUMBER, false,
                                                false, NSColorSpace.CalibratedRGB, pixelWidth * PIXEL_BYTES, 0);
            //var imageRep = new NSBitmapImageRep(new RectangleF(PointF.Empty, size));
            var image = new NSImage(size);

            var film = new ColorSurfaceFilm(pixelWidth, pixelHeight, 1);
            var scene = new SimpleScene().CreateScene(film);
            scene.Render();

            var imageBuffer = new NSBitmapImageRepImageBuffer(imageRep);
            film.Present(imageBuffer);

            image.AddRepresentation(imageRep);
            ImageView.Image = image;

            OutputTextField.StringValue = "Hello World!";
        }
    }
}

