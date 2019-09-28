diverging.colormap<-function(s,rgb1,rgb2, outColorspace='sRGB')
{
    # This function is based on Kenneth Moreland's code for creating Diverging Colormaps.
    # Matlab code created by Andy Stein. Translated to R by Jose Gama.
    # s is a vector that goes between zero and one
    # rgb1,rgb2 are objects from the colorspace package
    # RGB, sRGB, HLS, HSV, LAB, LUV, PolarLAB, PolarLUV, XYZ
    # outColorspace is the color space for the output
    library('colorspace', character.only=TRUE)
    LabToMsh<-function(Lab) 
    {
        L<-Lab@coords[1]
        a<-Lab@coords[2]
        b<-Lab@coords[3]
        M <- sqrt(L*L + a*a + b*b)
        s <- (M > 0.001) * acos(L/M)
        h <- (s > 0.001) * atan2(b,a)
        c(M,s,h)
    }
    MshToLab<-function(Msh)
    {
    M<-Msh[1]
    s<-Msh[2]
    h<-Msh[3]
    L <- M*cos(s)
    a <- M*sin(s)*cos(h)
    b <- M*sin(s)*sin(h)
    colorspace::LAB(L,a,b)
    }

    AngleDiff<-function(a1, a2)
    {
        # Given two angular orientations, returns the smallest angle between the two.
        v1<-matrix(c(cos(a1), sin(a1)),1,2,byrow=TRUE)
        v2<-matrix(c(cos(a2), sin(a2)),1,2,byrow=TRUE)
        x<-acos(sum(v1 * v2))
        x
    }
    AdjustHue<-function(msh, unsatM)
    {
        # For the case when interpolating from a saturated color to an unsaturated
        # color, find a hue for the unsaturated color that makes sense.
        if (msh[1] >= unsatM-0.1  ) {
            # The best we can do is hold hue constant.
            h <- msh[3]
        } else {
            # This equation is designed to make the perceptual change of the interpolation to be close to constant.
            hueSpin <- (msh[2]*sqrt(unsatM^2 - msh[1]^2)/(msh[1]*sin(msh[2])))
            # Spin hue away from 0 except in purple hues.
            if (msh[3] > -0.3*pi) h <- msh[3] + hueSpin else h <- msh[3] - hueSpin
        }
        h
    }
    diverging.map.1val<-function(s, rgb1, rgb2, outColorspace='sRGB')
    {
        # Interpolate a diverging color map
        # s is a number between 0 and 1
        msh1 <- LabToMsh(as(rgb1, "LAB"))
        msh2 <- LabToMsh(as(rgb2, "LAB"))
        # If the endpoints are distinct saturated colors, then place white in between them
        if (msh1[2] > 0.05 & msh2[2] > 0.05 & AngleDiff(msh1[3],msh2[3]) > pi/3)
        {
            # Insert the white midpoint by setting one end to white and adjusting the scalar value.
            Mmid <- max(88.0, msh1[1], msh2[1])
            #Mmid <- max(Mmid)
            if (s < 0.5)
            {
                msh2[1] <- Mmid;  msh2[2] <- 0.0;  msh2[3] <- 0.0;s <- 2.0*s
            } else {
                msh1[1] <- Mmid;  msh1[2] <- 0.0;  msh1[3] <- 0.0; s <- 2.0*s - 1.0
            }
        }
        # If one color has no saturation, then its hue value is invalid.  In this
        # case, we want to set it to something logical so that the interpolation of hue makes sense.
        if ((msh1[2] < 0.05) & (msh2[2] > 0.05)) {
            msh1[3] <- AdjustHue(msh2, msh1[1]) 
        } else if ((msh2[2] < 0.05) & (msh1[2] > 0.05)) {
            msh2[3] <- AdjustHue(msh1, msh2[1])
        }
        mshTmp<-msh1
        mshTmp[1] <- (1-s)*msh1[1] + s*msh2[1]
        mshTmp[2] <- (1-s)*msh1[2] + s*msh2[2]
        mshTmp[3]<- (1-s)*msh1[3] + s*msh2[3]
        # Now convert back to the desired color space
        as(MshToLab(mshTmp),outColorspace)
    }
    dvmap<-matrix(0,length(s),3)
    for (n in 1:length(s)) dvmap[n,]<-diverging.map.1val(s[n], rgb1, rgb2, outColorspace)@coords
    dvmap
}
