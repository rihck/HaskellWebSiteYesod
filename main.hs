{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)
import Yesod.Static

data Pagina = Pagina{connPool :: ConnectionPool,
                     getStatic :: Static 
                    }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Animals 
   nome Text sqltype=varchar(10)
   idade Int
   racaid RacaId
   deriving Show
Users 
   nome Text sqltype=varchar(50)
   login Text sqltype=varchar(50)
   senha Text sqltype=varchar(50)
   deriving Show
Raca
   nome Text sqltype=varchar(50)
   apelido Text sqltype=varchar(10)
   deriving Show
   
Servico
    nome Text sqltype=varchar(50)
    descricao Text sqltype=varchar
    valor Double
    deriving Show
--Servico_Realizado
--   AnimalsId
--   ServicoId
|]
staticFiles "static"
mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/animal/checar/#AnimalsId ChecarAnimalR GET POST
/erro ErroR GET

/login LoginR GET POST
/usuario/cadastro UsuarioR GET POST
/admin AdminR GET
/inicio InicioR GET
/logout LogoutR GET

/sistema/meuperfil MeuPerfilR GET
/sistema/animal/cadastro AnimalR GET POST
/sistema/animal/listar ListarAnimalR GET

/admin/animal/raca/cadastro RacaR GET POST
/admin/perfil/#UsersId PerfilR GET POST
/admin/usuario/listar ListarUsuarioR GET
/static StaticR Static getStatic

/animal/servico ServicoR GET POST
|]

instance Yesod Pagina where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized ListarUsuarioR _ = isAdmin
    isAuthorized (PerfilR _) _ = isAdmin
    isAuthorized RacaR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Acesso Restrito para Administrador"

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage
------------------------

formRaca :: Form Raca
formRaca = renderDivs $ Raca <$>
            areq textField FieldSettings{fsId=Just "hident1",
                           fsLabel="Nome: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Nome"),("maxlength","50")]} Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Apelido",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Apelido"),("maxlength","10")]} Nothing
                           
-- Sempre que preciso um form, sera ncessario
-- funcoes deste tipo
formAnimal :: Form Animals
formAnimal = renderDivs $ Animals <$>
            areq textField FieldSettings{fsId=Just "hident1",
                           fsLabel="Nome: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Nome"),("maxlength","50")]} Nothing <*>
            areq intField FieldSettings{fsId=Just "hident2",
                           fsLabel="Idade: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Idade"),("max","999")]} Nothing <*>
            areq (selectField racas) FieldSettings{fsId=Just "hident3",
                           fsLabel="Raça: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control")]} Nothing

racas = do
       entidades <- runDB $ selectList [] [Asc RacaNome] 
       optionsPairs $ fmap (\ent -> (racaNome $ entityVal ent, entityKey ent)) entidades
           
formUser :: Form Users
formUser = renderDivs $ Users <$>
            areq textField FieldSettings{fsId=Just "hident1",
                               fsLabel="Nome: ",
                               fsTooltip= Nothing,
                               fsName= Nothing,
                               fsAttrs=[("class","form-control"),("placeholder","Nome"),("maxlength","50")]} Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                               fsLabel="Username: ",
                               fsTooltip= Nothing,
                               fsName= Nothing,
                               fsAttrs=[("class","form-control"),("placeholder","Username"),("maxlength","50")]} Nothing <*>
            areq passwordField FieldSettings{fsId=Just "hident3",
                               fsLabel="Senha: ",
                               fsTooltip= Nothing,
                               fsName= Nothing,
                               fsAttrs=[("class","form-control"),("placeholder","Senha"),("maxlength","50")]} Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
            areq textField FieldSettings{fsId=Just "hident1",
                           fsLabel="Username: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Username"),("maxlength","50")]} Nothing <*>
            areq passwordField FieldSettings{fsId=Just "hident2",
                           fsLabel="Senha: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Senha"),("maxlength","50")]} Nothing 

--TODO: Adicionar/Criar Layout Tela de Cadastro de Serviços
formServico :: Form Servico
formServico = renderDivs $ Servico <$>
            areq textField FieldSettings{fsId=Just "hident1",
                           fsLabel="Nome: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Nome"),("maxlength","50")]} Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                               fsLabel="Descrição: ",
                               fsTooltip= Nothing,
                               fsName= Nothing,
                               fsAttrs=[("class","form-control"),("placeholder","Descrição"),("maxlength","50")]} Nothing <*>
            areq doubleField FieldSettings{fsId=Just "hident3",
                           fsLabel="Valor: ",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Valor")]} Nothing
                               
           
getRacaR :: Handler Html
getRacaR = do
           (widget, enctype) <- generateFormPost formRaca
           defaultLayout $ do
                    addStylesheet $ StaticR css_components_css
                    addStylesheet $ StaticR css_background_admin_css
                    addStylesheet $ StaticR css_bootstrap_min_css
                    addScript $ StaticR js_jquery_2_2_4_min_js
                    addScript $ StaticR js_bootstrap_js
                    toWidget $ $(whamletFile "templates/raca.hamlet")

getAnimalR :: Handler Html
getAnimalR = do
        (widget, enctype) <- generateFormPost formAnimal
        key <- lookupSession "_ID"
        case key of
            Just x -> do 
                let campo = toSqlKey $ read $ unpack x
                user <- runDB $ selectFirst [UsersId ==. campo] []
                defaultLayout $ do
                    addStylesheet $ StaticR css_components_css
                    addStylesheet $ StaticR css_background_css
                    addStylesheet $ StaticR css_bootstrap_min_css
                    addScript $ StaticR js_jquery_2_2_4_min_js
                    addScript $ StaticR js_bootstrap_js
                    toWidget $ $(whamletFile "templates/animal.hamlet")
            Nothing -> redirect HomeR
           
getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
        user <- runDB $ get404 uid
        defaultLayout $ do
                    addStylesheet $ StaticR css_components_css
                    addStylesheet $ StaticR css_background_css
                    addStylesheet $ StaticR css_bootstrap_min_css
                    addScript $ StaticR js_jquery_2_2_4_min_js
                    addScript $ StaticR js_bootstrap_js
                    toWidget $ $(whamletFile "templates/perfil.hamlet")
                    
getMeuPerfilR :: Handler Html
getMeuPerfilR = do
        key <- lookupSession "_ID"
        case key of
            Just x -> do 
                let campo = toSqlKey $ read $ unpack x
                user <- runDB $ selectFirst [UsersId ==. campo] []
                defaultLayout $ do
                    addStylesheet $ StaticR css_components_css
                    addStylesheet $ StaticR css_background_css
                    addStylesheet $ StaticR css_bootstrap_min_css
                    addScript $ StaticR js_jquery_2_2_4_min_js
                    addScript $ StaticR js_bootstrap_js
                    toWidget $ $(whamletFile "templates/meuperfil.hamlet")
            Nothing -> redirect HomeR
          
getInicioR :: Handler Html
getInicioR = do
        key <- lookupSession "_ID"
        case key of
            Just x -> do 
                let campo = toSqlKey $ read $ unpack x
                user <- runDB $ selectFirst [UsersId ==. campo] []
                defaultLayout $ do
                    addStylesheet $ StaticR css_components_css
                    addStylesheet $ StaticR css_background_css
                    addStylesheet $ StaticR css_bootstrap_min_css
                    addScript $ StaticR js_jquery_2_2_4_min_js
                    addScript $ StaticR js_bootstrap_js
                    toWidget $ $(whamletFile "templates/inicio.hamlet")
            Nothing -> redirect HomeR

getUsuarioR :: Handler Html
getUsuarioR = do
            (widget, enctype) <- generateFormPost formUser
            defaultLayout $ do
                addStylesheet $ StaticR css_components_css
                addStylesheet $ StaticR css_background_css
                addStylesheet $ StaticR css_bootstrap_min_css
                addScript $ StaticR js_jquery_2_2_4_min_js
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(whamletFile "templates/cadastrar.hamlet")
                
getServicoR :: Handler Html
getServicoR = do
        (widget, enctype) <- generateFormPost formServico
        defaultLayout $ do
            addStylesheet $ StaticR css_components_css
            addStylesheet $ StaticR css_background_admin_css
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_jquery_2_2_4_min_js
            addScript $ StaticR js_bootstrap_js
            toWidget $ $(whamletFile "templates/servico.hamlet") 
            --TODO: Criar Stilo para Tela de Cadastro de Servico (Copiado o do Cadastro pra Teste)
           
getListarAnimalR :: Handler Html
getListarAnimalR = do
            listaAnm <- runDB $ selectList [] [Asc AnimalsNome]
            key <- lookupSession "_ID"
            case key of
                Just x -> do 
                    let campo = toSqlKey $ read $ unpack x
                    user <- runDB $ selectFirst [UsersId ==. campo] []
                    defaultLayout $ do
                        addStylesheet $ StaticR css_components_css
                        addStylesheet $ StaticR css_background_css
                        addStylesheet $ StaticR css_bootstrap_min_css
                        addScript $ StaticR js_jquery_2_2_4_min_js
                        addScript $ StaticR js_bootstrap_js
                        toWidget $ $(whamletFile "templates/listaAnimal.hamlet")
                Nothing -> redirect HomeR

getListarUsuarioR :: Handler Html
getListarUsuarioR = do
            listaUsr <- runDB $ selectList [] [Asc UsersNome]
            addStylesheet $ StaticR css_components_css
            addStylesheet $ StaticR css_background_admin_css
            addStylesheet $ StaticR css_bootstrap_min_css
            addScript $ StaticR js_jquery_2_2_4_min_js
            addScript $ StaticR js_bootstrap_js
            toWidget $ $(whamletFile "templates/admin.hamlet")
{-             defaultLayout $ [whamlet|
                 <h1> Usuarios Cadastrados no Sistema:
                 $forall Entity pid users <- listaUsr
                     <a href=@{PerfilR pid}> #{usersNome users} 
                     <form method=post action=@{PerfilR pid}> 
                         <input type="submit" value="Deletar User"><br>
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]-}
           
postRacaR :: Handler Html
postRacaR = do
                ((result, _), _) <- runFormPost formRaca
                case result of
                    FormSuccess raca -> do
                       runDB $ insert raca
                       defaultLayout [whamlet|
                           <h1> A inserção da Raça #{racaNome raca} foi concluida com sucesso. 
                       |]
                    _ -> redirect RacaR
                    
postChecarAnimalR :: AnimalsId -> Handler Html
postChecarAnimalR pid = do
     runDB $ delete pid
     redirect ListarAnimalR
     
postPerfilR :: UsersId -> Handler Html
postPerfilR pid = do
     runDB $ delete pid
     redirect ListarUsuarioR

postAnimalR :: Handler Html
postAnimalR = do
           ((result, _), _) <- runFormPost formAnimal
           case result of 
               FormSuccess anim -> (runDB $ insert anim) >>= \piid -> redirect (ChecarAnimalR piid)
               _ -> redirect ErroR
               
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect LoginR
               _ -> redirect ErroR
               
postServicoR :: Handler Html
postServicoR = do
           ((result, _), _) <- runFormPost formServico
           case result of 
               FormSuccess anim -> (runDB $ insert anim) >>= \piid -> redirect InicioR
               _ -> redirect ErroR
               
getHomeR :: Handler Html
getHomeR =  defaultLayout $ do
                addStylesheet $ StaticR css_components_css
                addStylesheet $ StaticR css_background_css
                addStylesheet $ StaticR css_bootstrap_min_css
                addScript $ StaticR js_jquery_2_2_4_min_js
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(whamletFile "templates/index.hamlet")
           
getAdminR :: Handler Html
getAdminR = defaultLayout $ do
                addStylesheet $ StaticR css_components_css
                addStylesheet $ StaticR css_background_admin_css
                addStylesheet $ StaticR css_bootstrap_min_css
                addScript $ StaticR js_jquery_2_2_4_min_js
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(whamletFile "templates/admin.hamlet")

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ do
                addStylesheet $ StaticR css_components_css
                addStylesheet $ StaticR css_background_css
                addStylesheet $ StaticR css_bootstrap_min_css
                addScript $ StaticR js_jquery_2_2_4_min_js
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(whamletFile "templates/login.hamlet")
           
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","eitapleuga") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect InicioR

getChecarAnimalR :: AnimalsId -> Handler Html
getChecarAnimalR pid = do
    animal <- runDB $ get404 pid
    defaultLayout $ do
                addStylesheet $ StaticR css_components_css
                addStylesheet $ StaticR css_background_admin_css
                addStylesheet $ StaticR css_bootstrap_min_css
                addScript $ StaticR js_jquery_2_2_4_min_js
                addScript $ StaticR js_bootstrap_js
                toWidget $ $(whamletFile "templates/admin.hamlet")

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    <h1>Falha no Cadastro !</h1>
|]


getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     redirect HomeR


connStr = "dbname=d4673as0stmsm7 host=ec2-54-221-225-242.compute-1.amazonaws.com user=nzjfptmglfomng password=fyYms4A9T8gkP4_Go8GswcfIiE port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       t@(Static settings) <- static "static"
       warp 8080 (Pagina pool t)
