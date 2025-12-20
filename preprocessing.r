convert_types <- function(data) {
  ## 0. 先保留一份原表，防止误伤
  result <- data

  ## 1. 建立“字典”：每个变量该是什么类型/水平
  type_dict <- list(
    #----- 有序因子（等级、质量、优良中差） -----
    OverallQual = ordered(c(1:10)),
    OverallCond = ordered(c(1:10)),
    ExterQual = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    ExterCond = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    BsmtQual = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    BsmtCond = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    BsmtExposure = ordered(c("No", "Mn", "Av", "Gd")),
    BsmtFinType1 = ordered(c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
    BsmtFinType2 = ordered(c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
    HeatingQC = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    KitchenQual = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    FireplaceQu = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    GarageQual = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    GarageCond = ordered(c("Po", "Fa", "TA", "Gd", "Ex")),
    PoolQC = ordered(c("Fa", "TA", "Gd", "Ex")),
    Fence = ordered(c("MnWw", "GdWo", "MnPrv", "GdPrv")),
    LandSlope = ordered(c("Gtl", "Mod", "Sev")),
    LotShape = ordered(c("IR3", "IR2", "IR1", "Reg")),
    Functional = ordered(c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")),

    #----- 普通因子 -----
    MSSubClass = factor(levels = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150, 160, 180, 190)),
    MSZoning = factor(c("A", "C", "FV", "I", "RH", "RL", "RP", "RM")),
    Street = factor(c("Grvl", "Pave")),
    Alley = factor(c("Grvl", "Pave", "NA")),
    LotConfig = factor(c("Inside", "Corner", "CulDSac", "FR2", "FR3")),
    LandContour = factor(c("Lvl", "Bnk", "HLS", "Low")),
    Utilities = factor(c("AllPub", "NoSewr", "NoSeWa", "ELO")),
    Neighborhood = factor(c(
      "Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr", "CollgCr",
      "Crawfor", "Edwards", "Gilbert", "IDOTRR", "MeadowV", "Mitchel",
      "Names", "NoRidge", "NPkVill", "NridgHt", "NWAmes", "OldTown",
      "SWISU", "Sawyer", "SawyerW", "Somerst", "StoneBr", "Timber", "Veenker"
    )),
    Condition1 = factor(c("Artery", "Feedr", "Norm", "RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe")),
    Condition2 = factor(c("Artery", "Feedr", "Norm", "RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe")),
    BldgType = factor(c("1Fam", "2FmCon", "Duplx", "TwnhsE", "TwnhsI")),
    HouseStyle = factor(c("1Story", "1.5Fin", "1.5Unf", "2Story", "2.5Fin", "2.5Unf", "SFoyer", "SLvl")),
    RoofStyle = factor(c("Flat", "Gable", "Gambrel", "Hip", "Mansard", "Shed")),
    RoofMatl = factor(c("ClyTile", "CompShg", "Membran", "Metal", "Roll", "Tar&Grv", "WdShake", "WdShngl")),
    Exterior1st = factor(c(
      "AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "CemntBd", "HdBoard",
      "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco",
      "VinylSd", "Wd Sdng", "WdShing"
    )),
    Exterior2nd = factor(c(
      "AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "CemntBd", "HdBoard",
      "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone", "Stucco",
      "VinylSd", "Wd Sdng", "WdShing"
    )),
    MasVnrType = factor(c("BrkCmn", "BrkFace", "CBlock", "None", "Stone")),
    Foundation = factor(c("BrkTil", "CBlock", "PConc", "Slab", "Stone", "Wood")),
    Heating = factor(c("Floor", "GasA", "GasW", "Grav", "OthW", "Wall")),
    Electrical = factor(c("SBrkr", "FuseA", "FuseF", "FuseP", "Mix")),
    GarageType = factor(c("2Types", "Attchd", "Basment", "BuiltIn", "CarPort", "Detchd", "NA")),
    GarageFinish = factor(c("Fin", "RFn", "Unf", "NA")),
    PavedDrive = factor(c("Y", "P", "N")),
    SaleType = factor(c("WD", "CWD", "VWD", "New", "COD", "Con", "ConLw", "ConLI", "ConLD", "Oth")),
    SaleCondition = factor(c("Normal", "Abnorml", "AdjLand", "Alloca", "Family", "Partial")),
    CentralAir = factor(c("N", "Y")), # 后面再转 logical

    #----- 逻辑变量 -----
    # CentralAir  单独处理

    #----- 显式日期/时间 -----
    YearBuilt = integer(),
    YearRemodAdd = integer(),
    YrSold = integer(),
    MoSold = integer(),

    #----- 纯数值（面积、长度、个数、价格） -----
    LotFrontage = numeric(),
    LotArea = numeric(),
    MasVnrArea = numeric(),
    BsmtFinSF1 = numeric(),
    BsmtFinSF2 = numeric(),
    BsmtUnfSF = numeric(),
    TotalBsmtSF = numeric(),
    X1stFlrSF = numeric(),
    X2ndFlrSF = numeric(),
    LowQualFinSF = numeric(),
    GrLivArea = numeric(),
    BsmtFullBath = integer(),
    BsmtHalfBath = integer(),
    FullBath = integer(),
    HalfBath = integer(),
    Bedroom = integer(),
    Kitchen = integer(),
    TotRmsAbvGrd = integer(),
    Fireplaces = integer(),
    GarageCars = integer(),
    GarageArea = numeric(),
    WoodDeckSF = numeric(),
    OpenPorchSF = numeric(),
    EnclosedPorch = numeric(),
    X3SsnPorch = numeric(),
    ScreenPorch = numeric(),
    PoolArea = numeric(),
    MiscVal = numeric()
  )

  ## 2. 按字典批量转换
  for (col in names(result)) {
    if (!col %in% names(type_dict)) next # 字典里没有就跳过

    spec <- type_dict[[col]]

    if (is.ordered(spec)) { # 有序因子
      result[[col]] <- ordered(result[[col]], levels = levels(spec))
    } else if (is.factor(spec)) { # 普通因子
      result[[col]] <- factor(result[[col]], levels = levels(spec))
    } else if (col == "CentralAir") { # Y/N → logical
      result[[col]] <- result[[col]] == "Y"
    } else if (identical(spec, integer())) { # 整型
      result[[col]] <- as.integer(result[[col]])
    } else if (identical(spec, numeric())) { # 双精度
      result[[col]] <- as.numeric(result[[col]])
    }
  }

  ## 3. 统一把 logical 转 0/1
  log_cols <- sapply(result, is.logical)
  result[log_cols] <- lapply(result[log_cols], as.numeric)

  return(result)
}

impute_missing <- function(data, ref_data = NULL) {
  result <- data
  # special meaning
  cols <- c("PoolQC", "Alley", "Fence", "MiscFeature")
  result[cols] <- lapply(result[cols], function(x) ifelse(is.na(x), "NA", x))
  for (col in names(result)) {
    if (any(is.na(result[[col]]))) {
      # 数值列：中位数填充
      if (is.numeric(result[[col]])) {
        if (!is.null(ref_data) && col %in% names(ref_data)) {
          fill_val <- median(ref_data[[col]], na.rm = TRUE)
        } else {
          fill_val <- median(result[[col]], na.rm = TRUE)
        }
        result[[col]][is.na(result[[col]])] <- fill_val

        # 因子列：众数填充
      } else if (is.factor(result[[col]])) {
        if (!is.null(ref_data) && col %in% names(ref_data)) {
          freq_table <- table(ref_data[[col]])
        } else {
          freq_table <- table(result[[col]])
        }
        if (length(freq_table) > 0) {
          mode_val <- names(freq_table)[which.max(freq_table)]
          result[[col]][is.na(result[[col]])] <- mode_val
        }
      }
    }
  }

  return(result)
}

#' 标准化连续变量
#' @param data 数据框
#' @param exclude_cols 需要排除的列名（如ID、分类变量等）
#' @return 标准化后的数据框
standardize_continuous <- function(data, exclude_cols = NULL) {
  result <- data
  
  # 1. 识别连续变量（数值型且不是逻辑变量）
  continuous_cols <- names(result)[
    sapply(result, function(x) is.numeric(x) && !is.logical(x))
  ]
  
  # 2. 排除指定列
  if (!is.null(exclude_cols)) {
    continuous_cols <- setdiff(continuous_cols, exclude_cols)
  }
  
  # 3. 对每个连续变量进行标准化（z-score标准化）
  for (col in continuous_cols) {
    if (length(unique(na.omit(result[[col]]))) > 1) { # 确保有变异度
      result[[col]] <- scale(result[[col]])
    } else {
      warning(paste("Column", col, "has zero variance, skipping standardization"))
    }
  }
  
  return(result)
}

preprocess_pipeline <- function(X) {
  # 去掉 Id
  if ("Id" %in% colnames(X)){
    X <- subset(X, select = -Id)
  }
  # 数字开头 -> 加 X
  names(X) <- sub("^(\\d)", "X\\1", names(X))
  X <- convert_types(X)
  X <- impute_missing(X)
  X <- standardize_continuous(X)
  return(X)
}

#' 无副作用的 One-Hot 编码，列名格式 name(c)
#' @param df      data.frame/data.table
#' @param na.str  缺失值映射字符串，默认 "NA"
#' @return        仅含数值列的全新 data.table
onehot_namec <- function(df, na.str = "NA") {
  # 用法
  library(data.table)
  dt <- data.table::as.data.table(df)[, ]

  ## 2. 找出所有 category 列
  cat_cols <- names(dt)[sapply(dt, function(x) is.factor(x) | is.character(x))]
  if (length(cat_cols) == 0) {
    return(dt)
  }

  ## 4. 逐列生成 dummy
  new_parts <- lapply(cat_cols, function(j) {
    fct <- factor(dt[[j]], levels = unique(sort(dt[[j]])))
    mat <- model.matrix(~ . - 1, data.frame(x = fct))
    colnames(mat) <- paste0(j, "(", levels(fct), ")")
    as.data.table(mat)
  })

  ## 5. 删除原 category 列，再真正拼接
  dt[, (cat_cols) := NULL]
  dt <- cbind(dt, do.call(cbind, new_parts)) # 关键修正

  setcolorder(dt, order(names(dt)))
  dt[]
}
