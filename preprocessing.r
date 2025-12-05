convert_types <- function(data) {
    result <- data

    # 字符转因子
    for (col in names(result)) {
        if (is.character(result[[col]])) {
            result[[col]] <- as.factor(result[[col]])
        }
        # 逻辑转数值
        if (is.logical(result[[col]])) {
            result[[col]] <- as.numeric(result[[col]])
        }
    }

    # 移除ID列
    if ("Id" %in% names(result)) {
        result <- result[, names(result) != "Id", drop = FALSE]
    }

    return(result)
}

impute_missing <- function(data, ref_data = NULL) {
    result <- data

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

preprocess_pipeline <- function(X) {
    X_train_conv <- convert_types(X)
    X_train_clean <- impute_missing(X_train_conv)
    return(X_train_clean)
}

main_preprocess <- function(x_train, x_val = NULL, x_test = NULL) {
    # 3. 主处理流程
    cat("数据预处理开始...\n")
    # 初始化结果
    result <- list(x_train = preprocess_pipeline(x_train))

    # 处理验证集（如果有）
    if (!is.null(x_val)) {
        result$x_val <- preprocess_pipeline(x_val)
    }

    # 处理测试集（如果有）
    if (!is.null(x_test)) {
        result$X_test <- preprocess_pipeline(x_test)
    }

    return(result)
}
