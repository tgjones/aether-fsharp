using System;
using System.Collections.Generic;
using System.Linq;
using MonoMac.Foundation;
using MonoMac.AppKit;
using System.Runtime.InteropServices;
using Aether.Studio.Shared;
using Aether.Films;
using Aether.Studio.Mac;

namespace TestDocument
{
    public partial class MyDocument : MonoMac.AppKit.NSDocument
    {
        const int SAMPLE_SIZE = 8;
        const int SAMPLE_NUMBER = 3;
        const int PIXEL_BYTES = 3;

        // Called when created from unmanaged code
        public MyDocument(IntPtr handle) : base (handle)
        {
        }
		
        // Called when created directly from a XIB file
        [Export ("initWithCoder:")]
        public MyDocument(NSCoder coder) : base (coder)
        {
        }

        public override void WindowControllerDidLoadNib(NSWindowController windowController)
        {
            base.WindowControllerDidLoadNib(windowController);
			
            // Add code to here after the controller has loaded the document window
            RenderImage(null);
        }
		
        // 
        // Save support:
        //    Override one of GetAsData, GetAsFileWrapper, or WriteToUrl.
        //
		
        // This method should store the contents of the document using the given typeName
        // on the return NSData value.
        public override NSData GetAsData(string documentType, out NSError outError)
        {
            outError = NSError.FromDomain(NSError.OsStatusErrorDomain, -4);
            return null;
        }
		
        // 
        // Load support:
        //    Override one of ReadFromData, ReadFromFileWrapper or ReadFromUrl
        //
        public override bool ReadFromData(NSData data, string typeName, out NSError outError)
        {
            outError = NSError.FromDomain(NSError.OsStatusErrorDomain, -4);
            return false;
        }

        // If this returns the name of a NIB file instead of null, a NSDocumentController 
        // is automatically created for you.
        public override string WindowNibName
        { 
            get
            {
                return "MyDocument";
            }
        }

        partial void RenderImage(NSObject sender)
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
        }
    }
}

