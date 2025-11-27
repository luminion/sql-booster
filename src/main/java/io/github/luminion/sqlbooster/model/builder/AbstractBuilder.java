package io.github.luminion.sqlbooster.model.builder;

import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.api.SqlContext;
import io.github.luminion.sqlbooster.model.api.Sort;
import io.github.luminion.sqlbooster.model.api.ConditionNode;
import io.github.luminion.sqlbooster.util.BoostUtils;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.Getter;

import java.util.Map;
import java.util.function.Function;

/**
 * 链式助手.
 * <p>
 * 提供了 SQL 构建的基本功能, 包括条件添加、排序设置等, 并通过泛型支持链式调用.
 *
 * @param <T> 实体类型
 * @param <S> 返回类型 (用于支持链式调用)
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings({"unused", "unchecked"})
public abstract class AbstractBuilder<T, S extends AbstractBuilder<T, S>>  extends SqlContext<T> {

    /**
     * 关联的实体类, 用于 SQL 校验和处理.
     */
    @Getter
    protected transient Class<T> entityClass;

    /**
     * 创建一个新的自身实例.
     *
     * @return 新实例
     * @since 1.0.0
     */
    public abstract S newInstance();
    
    /**
     * 应用一个处理器对当前的 SQL 助手进行转换或处理.
     *
     * @param processor 处理器函数
     * @return this
     * @since 1.0.0
     */
    public S build(Function<S, S> processor){
        return processor.apply((S) this);
    }
    
    /**
     * 设置关联的实体类.
     *
     * @param entityClass 实体类
     * @return 当前实例
     * @since 1.0.0
     */
    public S entity(Class<T> entityClass){
        this.entityClass = entityClass;
        return (S) this;
    }

    /**
     * 从目标对象加载查询条件.
     *
     * @param object 目标对象
     * @return 当前实例
     * @since 1.0.0
     */
    public S append(Object object) {
        if (object == null) {
        }
        else if (object instanceof Condition){
            this.getConditions().add((Condition) object);
        }
        else if (object instanceof Sort){
            this.getSorts().add((Sort) object);
        }
        else if (object instanceof ConditionNode){
            super.merge((ConditionNode) object);
        }
        else{
            Map<?, ?> map = ReflectUtils.objectToMap(object);
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                Object key = entry.getKey();
                Object value = entry.getValue();
                Condition condition = new Condition(key.toString(), value);
                this.getConditions().add(condition);
            }
        }
        return (S) this;
    }

    /**
     * 转换为 {@link BoostBuilder}.
     *
     * @param boosterCore {@link BoosterCore} 实例
     * @param <V>       VO 类型
     * @param <P>       分页对象类型
     * @return {@link BoostBuilder} 实例
     * @since 1.0.0
     */
    public <V, P> BoostBuilder<T, V> boost(BoosterCore<T, V> boosterCore) {
        this.entityClass = BoostUtils.getEntityClass(boosterCore);
        return new BoostBuilder<>(boosterCore).append(this);
    }

}