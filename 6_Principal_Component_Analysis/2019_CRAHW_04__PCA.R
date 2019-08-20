##############################################
# CRAHW R Workshop: PCA example script
# 21/08/2019
# R version 3.5.2 (2018-12-20)
# erin.walsh@anu.edu.au

## SETUP:
  data(iris)        # Example dataset
  library(ggplot2)  # version 2_3.1.0   for nicer visualisations

  # Removing the categorical variable
  PCA_dat<-iris[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
    # Side note: if you have categorical variables, look into Multiple Factor Analysis for mixed data, 
    # have heard good things about the FactoMineR package 
  
  
## STEP 1:
# Standardization (conversion to z scores)
#    Homogenizes range so they contribute equally to analysis

    # Range is not homogenized: 
      # - line of best fit looks steeper
      # - Petal.Length will contribute more to the variance as it has bigger range
    ggplot(PCA_dat, aes(x=Sepal.Length, y=Petal.Length)) + geom_point() + coord_fixed() +
            geom_smooth(method="lm", se = FALSE)
    
    # Range is homogenized using scale() to convert both to a z score
      # - Line of best fit is more realistic
      # - Both contribute equally to the variance
    ggplot(PCA_dat, aes(x=scale(Sepal.Length), y=scale(Petal.Length))) + geom_point() + coord_fixed() +
           geom_smooth(method="lm", se = FALSE)

    
  # You can do this at the point of the dataset
    
    PCA_dat_scaled<-scale(PCA_dat) 
    PCA_dat_scaled<-data.frame(scale(PCA_dat)) # Neater output if you wrap it in data.frame
    
  # Or at the point of the PCA using "scale=TRUE"  

    pca_notscaled<-prcomp(PCA_dat, scale=FALSE)          # No scaling
    pca_scaled_dat<-prcomp(PCA_dat_scaled, scale=FALSE)  # Scaled at the point of the dataset
    pca_scaled_pc<-prcomp(PCA_dat, scale=TRUE)           # Scaled at the point of the PCA fitting

    
    
    
## STEP 2:
# Covariance matrix computation
    
    # The prcomp() function does this for you, but it is good to know how it works...
        # Remembe this is not correlation: 
        # Correlation is the direction AND strength of the relationship
        # Covariance is just the direction of the relationship
        # The diagonal in cov() is the variance for the corresponding variable
    
    cov(PCA_dat)
    
        # So here we can see that Sepal.Width is the odd one out (negative relationship)
        # and that Petal.Length has the highest variance
    
    
 ## STEP 3 and 4 at the same time because prcomp() does them both for you (plus taking a look at what comes out):
    
    # Any order is fine (once fit, the model is fit) but I find this way most systematic:
    
    # a)  Look at eigenvalues (amount of variation in the original data that each component accounts for)
        
        pca_scaled_pc$sdev^2 # We need to square this 
          # Hmm, not too great: first component only accounts for 3% of variability -_-
        
        # So how many components do we want to retain? Look for the 'elbow'
        # Base R plot function knows how to do a scree plot this for you (so convenient!)
        
          plot(pca_scaled_pc)  
        
          
    # b)  Look at eigenvectors aka loading scores: how much each original variable contributes to each component
          
          pca_scaled_dat$rotation 
          
          # This is where you can really see the difference between scaling or not first:
          
          pca_notscaled$rotation  # Petal.Length overwhelms PC1 if you don't scale (because it has the highest variance)
          
          plot(pca_notscaled) # This, in turn, makes the first component unduly huge
          
        
          
     # c)  Look at each person's position on each component     
          
        pca_scaled_pc$x 
        
        # To plot it using base ggplot
            plot_dat<-data.frame( pca_scaled_pc$x)
            ggplot(plot_dat, aes(x=PC1, y=PC2)) + geom_point()
            ggplot(plot_dat, aes(x=PC2, y=PC3)) + geom_point()
            
        # But there is a more convenient way!
        
        library(ggfortify) # This package makes plotting very quick and easy
        
            autoplot(pca_scaled_pc) # One step
            
            # Also can conveniently add in eigenvectors
            autoplot(pca_scaled_pc, loadings = TRUE, loadings.label = TRUE)
            
            # And colour code based on factors, e.g.
            autoplot(pca_scaled_pc, loadings = TRUE, loadings.label = TRUE, data = iris, colour = 'Species')
            # (this is really useful to see if clusters form following other groupings in data)
            
            
          # And just to show it doesn't matter when you scale: this is identical
            
            autoplot(pca_scaled_dat, loadings = TRUE, loadings.label = TRUE, data = iris, colour = 'Species')
            
          # But it does come out different if you do not scale!
            autoplot(pca_notscaled, loadings = TRUE, loadings.label = TRUE, data = iris, colour = 'Species')
            
            
## STEP INFINITY: What about rotations, though?
            
        # PCA intends to maximise variability explained by original data, so is oblimin by default.
        # You can manually varimax rotate after fitting: they then become rotated, not principal components.
            
            
        # Compute eigenvalues and eigen vectors of the correlation matrix.
            eigenvalues<-eigen(cor(PCA_dat))
           
        # Select how many components you want to rotate (here, same number as unrotated)
            factors<- ncol( pca_scaled_pc$rotation )
            
            # Extract the eigenvalues

            
            rotated_comps<-eigenvalues$vectors [ , 1:factors ]  %*% 
                                + diag ( sqrt (eigenvalues$values [ 1:factors ] ),factors,factors )
            
        # So now your components are rotated so they may be correlated: note directions are the same    
            pca_scaled_pc$rotation      
            rotated_comps
            
    #... Or you could just use the psych() package.
            library(psych)
            
            principal(cov(PCA_dat),nfactors=4, rotate="oblimin") 
            # Should give you the same as rotated_comps
            
            
            principal(cov(PCA_dat),nfactors=4, rotate="varimax") 
            # and off we go!
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
## Over to you:
     data(mtcars)        # Example dataset
     dat<-mtcars[,c("mpg","cyl","disp","hp", "drat","wt","qsec")] # Subset for your convenience      
            
            
## PART 1:
# Standardization

                        
## PART 2:
# Covariance matrix computation
            
## PART 3:
# Fit your PCA
            
## PART 4: 
# Evaluate your PCA
    # a)  Look at eigenvalues (amount of variation in the original data that each component accounts for)
            
    # b)  Look at eigenvectors aka loading scores: how much each original variable contributes to each component
            
    # c)  Look at each person's position on each component              
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
