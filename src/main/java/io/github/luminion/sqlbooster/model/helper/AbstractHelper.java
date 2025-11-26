package io.github.luminion.sqlbooster.model.helper;


import io.github.luminion.sqlbooster.model.api.Wrapper;
import lombok.Getter;

import java.util.function.Function;

/**
 * SQL 构建助手接口.
 * <p>
 * 定义了 SQL 构建助手的基本功能, 包括获取实体类、应用处理器等.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@Getter
public abstract class AbstractHelper<T> extends Wrapper<T> {
    /**
     * 关联的实体类, 用于 SQL 校验和处理.
     */
    protected transient Class<T> entityClass;
    
    
    /**
     * 应用一个处理器对当前的 SQL 助手进行转换或处理.
     *
     * @param processor 处理器函数
     * @return 处理后的 {@link AbstractHelper}
     * @since 1.0.0
     */
    public AbstractHelper<T> process(Function<AbstractHelper<T>, AbstractHelper<T>> processor){
        return processor.apply(this);
    }
    
}