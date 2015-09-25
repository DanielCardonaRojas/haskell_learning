{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Applicative ((<*>), (<$>))
import Data.Text (pack,append) 
import qualified Data.Text.Lazy.IO as B
--Custom
import MedellinGourmetCSV 


defInfo = Info {logo = "images/restaurantes/milogo.png",
                  dir = "Calle 10 A # 43", 
                  tel = "234 4465",
                  cat = "Parrilla", 
                  val = "$59.000",
                  restr = "No aplica fines de semana",
                  nTab = "20",
                  web = Just "www.mirest.com",
                  fb = Just "www.fb.com",
                  twitter = Just "www.tw.com",
                  instagram = Just "www.insta.com",
                  tripadvisor = Nothing,
                  hasCoffee = True,
                  datos = ["entrada1","plato1","postre1",
                           "entrada2","plato2","postre2",
                           "entrada3","plato3","postre3"]
                 }

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

---------------- Reader Monad Helpers --------------------

direccionR, telefonoR, logoR, categoriaR,valorR,numMesasR,restricR :: Reader Info String
logoR = reader logo
direccionR = reader dir
telefonoR = reader tel
categoriaR = reader cat
valorR = reader val
numMesasR = reader nTab
restricR = reader restr

paginaWebR, facebookR, twitterR,instagramR,tripadvisorR :: Reader Info (Maybe String)
paginaWebR = reader web
facebookR = reader fb
twitterR = reader twitter
instagramR = reader instagram
tripadvisorR = reader tripadvisor

datosR:: Reader Info [String]
datosR = reader datos

socialOpts = do 
  f <- facebookR
  t <- twitterR
  i <- instagramR
  tr <- tripadvisorR
  return [f,t,i,tr]

cafeR :: Reader Info Bool
cafeR = reader hasCoffee

 ------------------------- LUCID HTML TEMPLATING ---------------------
iconosMenu :: Html ()
iconosMenu = div_ [class_ "row buttons-top hidden-xs hidden-sm"] $ do 
               div_ [class_ "col-xs-12"] $ do
                 div_ [class_ "icons-rest"] $ do
                   img_ [src_ "/images/menu/vino.jpg"]
                   img_ [src_ "/images/menu/mas.jpg" ]
                   img_ [src_ "/images/menu/entrada.jpg"]
                   img_ [src_ "/images/menu/mas.jpg" ]
                   img_ [src_ "/images/menu/plato-fuerte.jpg"]
                   img_ [src_ "/images/menu/mas.jpg" ]
                   img_ [src_ "/images/menu/postre.jpg"]
                   

-- VALOR, NUMERO MESAS, RESTRICCIONES

infoSecundaria :: HtmlT (Reader Info) ()
infoSecundaria = do 
  n <- lift numMesasR
  v <- lift valorR
  r <- lift restricR 
  div_ [class_ "row"] $ do 
     div_ [class_ "col-xs-12"] $ do
      p_ (toHtml "Número de mesas en su restaurante: " >> (span_ $ toHtml n))
  div_ [class_ "row-topper"] $ do 
    div_ [class_ "col-sm-8 col-xs-12 text-left"] $ do
      p_ ((strong_ "Restricciones") >> toHtml r)
    div_ [class_ "col-sm-4 col-xs-12 text-center"] $ do 
      div_ [class_ "reserva"] ("Valor del Menu" >> span_ (toHtml v <> "(incluye ICO)"))


iconoSocial :: Monad m => String -> String -> HtmlT (m) ()
iconoSocial icono link = 
  let source = "/images/menu/sociales/" `append` (pack icono) `append` ".png"
  in div_ [class_ "address-locale"] $ do
      a_ [href_ (pack link)] $ do
        img_ [style_ "float: left; margin-right: 15px;", src_ source]


mZip ls = mconcat . Prelude.zipWith (fmap) ls 

infoSocial :: Monad m => [Maybe String] -> Maybe (HtmlT m ())
infoSocial = mZip (map iconoSocial ["facebook","twitter","instagram","trip"]) 

iconoTexto :: Monad m => String -> String -> HtmlT (m) ()
iconoTexto icono t = 
  let source = "/images/iconos/" `append` (pack icono) `append` ".png"
  in div_ [class_ "address-locale"] $ do
      img_ [style_ "float: left; margin-right: 15px;", src_ source]
      span_ [style_ "font-size: 13px;"] (toHtml t)

-- LOGO, TELEFONO,DIRECCION, PAGINA WEB
infoBasica' :: HtmlT (Reader Info) ()
infoBasica' = do
  logo <- lift logoR
  tel <- lift telefonoR
  web <- lift paginaWebR
  direccion <- lift direccionR
  div_ [class_ "col-sm-3 col-xs-12"] $ do
    div_ [class_ "contain-image"] (img_ [src_ $ pack logo])
    mapM_ (uncurry iconoTexto) [("pin", direccion), ("telefono", tel)]
    maybe (return ()) (iconoTexto "w") web
    
     
--Cada 3 datos es una fila/opcion en el menu.                                          
--menuCarta :: Monad m => HtmlT m ()
menuCarta :: HtmlT (Reader Info) ()
menuCarta = 
  let 
    filas d = splitEvery 3 d
    mkRow s = tr_ $ mapM_ (td_ . toHtml) s :: Monad m => HtmlT m ()
  in  div_ [class_ "table-responsive"] $ do
        div_ [class_ "table table-bordered"] $ do 
         table_ [class_ "table table-bordered"] $ do 
           thead_ (mkRow ["Entrada","Plato Fuerte","Postre"]) 
           tbody_ $ do 
             r <- lift datosR
             (mapM_ mkRow (filas r))

--ICONOS CAFE POSTRE ETC.
iconsMenu :: Monad m => HtmlT (m) ()      
iconsMenu =
  let 
     source icono = "/images/menu/" `append` (pack icono) `append` ".jpg"
     imgSrc t = img_ [src_ t]
  in div_ [class_ "row buttons-top hidden-xs hidden-sm"] $ do
       div_ [class_ "col-xs-12"] $ do
         div_ [class_ "icons-rest"] $ do
           mapM_ (imgSrc . source) ["vino","mas","entrada","mas","plato-fuerte","mas","postre"]

-- CATEGORIA RESTAURANTE
restCat :: HtmlT (Reader Info) ()
restCat = do
  c <- lift categoriaR
  div_ [class_ "row"] $ do
    div_ [class_ "col-xs-12 categorie"] $ do
      div_ [class_ "page-header"] (toHtml c)

-------------------- MAIN TEMPLATE FUNCION ---------------

--runReader (renderTextT (restaurante)) defInfo 
restaurante :: HtmlT (Reader Info) ()
restaurante = div_ [class_ "row"] $ do
  infoBasica'
  opts <- lift socialOpts
  maybe mempty id (infoSocial opts)
  div_ [class_ "col-sm-9 col-xs-12"] $ do
    iconsMenu
    div_ [class_ "row space all-menu"] $ do
       restCat --opcional
       menuCarta
  infoSecundaria
  
----------------- CSV ----------------

main = do 
  i <- (getCSVInfo "mgourmet.csv")
  let i' = fmap id i 
  print i'
  case i' of
    Nothing -> putStrLn "Couldnt parse the csv file"
    Just a -> do 
           let htmlArticle = runReader (renderTextT restaurante) a                  
           B.writeFile "htmlOutput.html" htmlArticle
           return ()






  