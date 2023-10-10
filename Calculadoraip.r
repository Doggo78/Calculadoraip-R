ip <- readline("Ingrese la ip formato x.x.x.x/x ")
split_ip <- strsplit(ip, "/")[[1]]
ipcorrect <- split_ip
options(warn=-1)
residuo <- function(residuo1) {
    residuo_1 <- residuo1 %% 2
    if (residuo_1 == 0) {
        residuo_1
    } else if (residuo_1 == 1) {
        residuo_1
    }
}
ipbinario <- function(mitad1) {
    mitad1st <- mitad1[1]
    primero <- c()
    i <- 0

    while (mitad1st >= 1 && i <= 8) {
        a <- residuo(as.integer(mitad1st))
        primero <- c(a, primero)
        mitad1st <- mitad1st / 2
        i <- i + 1
    }
    mitad1st <- mitad1[2]
    segundo <- c()
    i <- 0
    while (mitad1st >= 1 && i <= 8) {
        b <- residuo(as.integer(mitad1st))
        segundo <- c(b, segundo)
        mitad1st <- mitad1st / 2
        i <- i + 1
    }
    mitad1st <- mitad1[3]
    tercero <- c()
    i <- 0
    while (mitad1st >= 1 && i <= 8) {
        c <- residuo(as.integer(mitad1st))
        tercero <- c(c, tercero)
        mitad1st <- mitad1st / 2
        i <- i + 1
    }
    mitad1st <- mitad1[4]
    cuarto <- c()
    i <- 0
    while (mitad1st >= 1 && i <= 8) {
        d <- residuo(as.integer(mitad1st))
        cuarto <- c(d, cuarto)
        mitad1st <- mitad1st / 2
        i <- i + 1
    }
    dirbase_1 <- dirbase(rellenar_con_ceros(primero), binarimask1) # nolint
    dirbase_2 <- dirbase((rellenar_con_ceros(segundo)), binarimask2) # nolint
    dirbase_3 <- dirbase((rellenar_con_ceros(tercero)), binarimask3) # nolint
    dirbase_4 <- dirbase((rellenar_con_ceros(cuarto)), binarimask4) # nolint
    dirbroat <- paste((c((paste(biadec((dirbroad(dirbase_1, resultadonmask1))), collapse = "")), (paste(biadec((dirbroad(dirbase_2, resultadonmask2))), collapse = "")), (paste(biadec((dirbroad(dirbase_3, resultadonmask3))), collapse = "")), (paste(biadec((dirbroad(dirbase_4, resultadonmask4))), collapse = "")))), collapse = ".") # nolint
    print(paste("Direccion broadcast: ", dirbroat)) # Direccion broadcast
    biadect <- paste((c((paste((biadec(dirbase_1)), collapse = "")), (paste((biadec(dirbase_2)), collapse = "")), (paste((biadec(dirbase_3)), collapse = "")), (paste((biadec(dirbase_4)), collapse = "")))), collapse = ".") # nolint
    print(paste("Dirección base: ", biadect)) # Direccion base
}
rellenar_con_ceros <- function(vector) {
    if (length(vector) < 8) {
        ceros_faltantes <- 8 - length(vector)
        vector_completo <- c(rep(0, ceros_faltantes), vector)
        return(vector_completo)
    } else if (length(vector) == 8) {
        return(vector)
    }
}
mask <- function(numero) {
    caracter <- "1"
    caractern <- strrep(caracter, numero)
    sepmask <- gsub("(.{8})", "\\1.", caractern)
    split_mask <- as.vector(strsplit(sepmask, "\\.| "))
    maskara <- as.numeric(as.character(unlist(split_mask)))
    par1cevt <- gsub("(.{1})", "\\1.", maskara[1])
    splitpar1 <- as.vector(strsplit(par1cevt, "\\.| "))
    unpar1 <- as.numeric(as.character(unlist(splitpar1)))
    unpar1st <- unpar1[1]
    if (is.na(unpar1st) == TRUE) {
        unpar1 <- c()
    }
    unpar1 <- rev(rellenar_con_ceros(unpar1))
    par2cevt <- gsub("(.{1})", "\\1.", maskara[2])
    splitpar2 <- as.vector(strsplit(par2cevt, "\\.| "))
    unpar2 <- as.numeric(as.character(unlist(splitpar2)))
    unpar2st <- unpar2[1]
    if (is.na(unpar2st) == TRUE) {
        unpar2 <- c()
    }
    unpar2 <- rev(rellenar_con_ceros(unpar2))
    par3cevt <- gsub("(.{1})", "\\1.", maskara[3])
    splitpar3 <- as.vector(strsplit(par3cevt, "\\.| "))
    unpar3 <- as.numeric(as.character(unlist(splitpar3)))
    unpar3st <- unpar3[1]
    if (is.na(unpar3st) == TRUE) {
        unpar3 <- c()
    }
    unpar3 <- rev(rellenar_con_ceros(unpar3))
    par4cevt <- gsub("(.{1})", "\\1.", maskara[4])
    splitpar4 <- as.vector(strsplit(par4cevt, "\\.| "))
    unpar4 <- as.numeric(as.character(unlist(splitpar4)))
    unpar4st <- unpar4[1]
    if (is.na(unpar4st) == TRUE) {
        unpar4 <- c()
    }
    unpar4 <- rev(rellenar_con_ceros(unpar4))
    binarimask1 <<- unpar1 # nolint
    binarimask2 <<- unpar2 # nolint
    binarimask3 <<- unpar3 # nolint
    binarimask4 <<- unpar4 # nolint
    resultadonmask1 <<- rellenar_con_ceros((notmask(unpar1))) # nolint
    resultadonmask2 <<- rellenar_con_ceros((notmask(unpar2))) # nolint
    resultadonmask3 <<- rellenar_con_ceros((notmask(unpar3))) # nolint
    resultadonmask4 <<- rellenar_con_ceros((notmask(unpar4))) # nolint
    resultado <- 0
    max_parametros <- 8
    for (i in seq_along((unpar1))) {
        if (unpar1[i] == 1) {
            resultado <- resultado + 2^(max_parametros - i)
        } else if (unpar1[i] == 0) {

        }
    }
    resultado2 <- 0
    for (i in seq_along((unpar2))) {
        if (unpar2[i] == 1) {
            resultado2 <- resultado2 + 2^(max_parametros - i)
        }
    }
    resultado3 <- 0
    for (i in seq_along((unpar3))) {
        if (unpar3[i] == 1) {
            resultado3 <- resultado3 + 2^(max_parametros - i)
        }
    }
    resultado4 <- 0
    for (i in seq_along((unpar4))) {
        if (unpar4[i] == 1) {
            resultado4 <- resultado4 + 2^(max_parametros - i)
        }
    }
}
notmask <- function(a) {
    mdefect <- c(1, 1, 1, 1, 1, 1, 1, 1)
    mask_sobrante <- xor(mdefect, a)
    mask_sobrante <<- c(as.integer(mask_sobrante))
}
dirbase <- function(a, b) {
    ipbase <- a & b
    ipbase <- c(as.integer(ipbase))
}
dirbroad <- function(a, b) {
    ipbroad <- xor(a, b)
    ipbroad <- c(as.integer(ipbroad))
}
biadec <- function(a) {
    resultado <- 0
    max_parametros <- 8
    for (i in seq_along(a)) {
        if (a[i] == 1) {
            resultado <- resultado + 2^(max_parametros - i)
        }
    }
    resultado
}
dsubnet <- function(numero) {
    numero <- as.integer(numero)
    cantidadsubred <- 2^(32 - numero)
    print(paste("Cantidad de subredes disponibles: ", cantidadsubred))
}
dhost <- function(numero) {
    numero <- as.integer(numero)
    hostdisponibles <- 2^(32 - numero) - 2
    print(paste("Cantidad de host disponibles: ", hostdisponibles))
}

if(length(ipcorrect) == 2 && is.numeric(ipcorrect) == TRUE){
    print("Formato correcto")
    maski <- split_ip[2]
    ip <- split_ip[1]
    split_ip <- as.vector(strsplit(ip, "\\.| "))
    ip <- as.numeric(as.character(unlist(split_ip)))
    dsubnet(maski)
    dhost(maski)
    mask(maski)
    ipbinario(ip)
}else{
    print("Ingrese un formato de IP correcto")
}
