package com.ccv2.fdf;

import java.awt.image.BufferedImage;
import java.awt.Image;
import java.net.URL;

/**
 * WindowsBmpFileReader converts a windows bmp file into an image
 */
public class WindowsBmpFileReader {

    // *** public constructors

    /**
     * create object from specified bmp file
     * @param fileName bmp file name
     */
    public WindowsBmpFileReader( String fileName) {
        reader = new WindowsBinaryFileReader(fileName);
        readData();
    }

    /**
     * create object from specified URL
     * @param url URL with bmp data
     */
    public WindowsBmpFileReader( URL url) {
        reader = new WindowsBinaryFileReader(url);
        readData();
    }

    // *** public methods

    /**
     * return bits per pixel
     * @return 1, 4, or 8
     */
    public int getBitsPerPixel() {
        return bitsPerPixel;
    }

    /**
     * return blue palette entry
     * @param entry palette subscript
     */
    public byte getBlue( int entry) {
        return blue [entry];
    }

    /**
     * return error message if invalid file
     * @return error message
     */
    public String getError() {
        return error;
    }

    /**
     * return green palette entry
     * @param entry palette subscript
     * @return green component of palette
     */
    public byte getGreen( int entry) {
        return green [entry];
    }

    /**
     * return # of palette entries
     * @return 2, 16, or 256
     */
    public int getPaletteSize() {
        return paletteSize;
    }

    /**
     * return pixel data
     * @param col column #
     * @param row row #
     * @return pixel value
     */
    public byte getPixelData( int col,  int row) {
        return pixelData [col] [row];
    }

    /**
     * return pixel height of image
     * @return height
     */
    public int getPixelHeight() {
        return pixelHeight;
    }

    /**
     * return pixel width of image
     * @return width
     */
    public int getPixelWidth() {
        return pixelWidth;
    }

    /**
     * return red palette entry
     * @param entry palette subscript
     * @return red component of palette
     */
    public byte getRed( int entry) {
        return red [entry];
    }

    // *** private methods

    /**
     * read bmp data
     */
    private void readData() {
        if (!readFileHeader()) return;
        if (!readInfoHeader()) return;
        if (!readPalette()) return;
        if (!readPixels()) return;
        reader.close();
    }

    /**
     * read bmp file header
     * @return false if not bmp file
     */
    private boolean readFileHeader() {
        if ((reader.readByte() != 'B') || (reader.readByte() != 'M')) {
            error = "BM not first 2 bytes of file";
            return false;
        }
        reader.skip(12);
        return true;
    }

    /**
     * read bmp info header
     * @return false if unable to process info header
     */
    private boolean readInfoHeader() {
        if (reader.readInt() != 40) {
            error = "InfoHeader size must be 40";
            return false;
        }
        pixelWidth = reader.readInt();
        pixelHeight = reader.readInt();
        reader.skip(2);
        bitsPerPixel = reader.readShort();
        if ((bitsPerPixel != 1) && (bitsPerPixel != 4) &&
            (bitsPerPixel != 8)) {
            error = "Bits per pixel not 1, 4, or 8";
            return false;
        }
        reader.skip(24);
        return true;
    }

    /**
     * read bmp palette
     * @return false if unable to process palette
     */
    private boolean readPalette() {
        switch (bitsPerPixel) {
            case 1 : paletteSize = 2; break;
            case 4 : paletteSize = 16; break;
            case 8 : paletteSize = 256; break;
        }
        for (int i = 0; i < paletteSize; i++) {
            blue [i] = reader.readByte();
            green [i] = reader.readByte();
            red [i] = reader.readByte();
            reader.readByte();
        }
        return true;
    }

    /**
     * read bmp pixel data
     * @return false if unable to process pixel data
     */
    private boolean readPixels() {
        pixelData = new byte [pixelWidth] [pixelHeight];
        int colIncrement = 8 / bitsPerPixel;
        int bytesPerRow = (pixelWidth + colIncrement - 1) / colIncrement;
        int fileBytesPerRow = (bytesPerRow + 3) / 4 * 4; // in file, byte count rounded up to be divisible by 4
        byte [] bmpRow = new byte [fileBytesPerRow];
        for (int row = pixelHeight - 1; row >= 0; row--) {
            reader.readBytes(bmpRow, fileBytesPerRow);
            int col = 0;
            for (int bmpCol = 0; bmpCol < bytesPerRow; bmpCol++) {
                switch (bitsPerPixel) {
                    case 1 :
                        for (int i = 7; i >= 0; i--) {
                            if ((col + i) >= pixelWidth) continue;
                            pixelData [col + i] [row] = (byte) (bmpRow [bmpCol] & 1);
                            bmpRow [bmpCol] = (byte) (bmpRow [bmpCol] >> 1);
                        }
                        col += 8;
                        break;
                    case 4 :
                        pixelData [col] [row] = (byte) ((bmpRow [bmpCol] >> 4) & 0xf);
                        if ((col + 1) < pixelWidth) {
                            pixelData [col + 1] [row] = (byte) (bmpRow [bmpCol] & 0xf);
                        }
                        col += 2;
                        break;
                    case 8 :
                        pixelData [col] [row] = bmpRow [bmpCol];
                        col++;
                        break;
                }
            }
        }
        return false;
    }

    // *** private data
    private int bitsPerPixel = 0; // 1, 4, or 8
    private byte [] blue = new byte [256]; // blue palette values
    private String error = ""; // error message if invalid file
    private byte [] green = new byte [256]; // green palette values
    private int paletteSize = 0; // number of palette entries (2, 16, or 256)
    private byte [] [] pixelData = null; // pixel data
    private int pixelHeight = 0; // image height
    private int pixelWidth = 0; // image width
    private byte [] red = new byte [256]; // red palette values
    WindowsBinaryFileReader reader = null; // object which actually reads the file

    // *** private classes

}