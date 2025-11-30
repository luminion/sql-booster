package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.ConditionSegment;
import io.github.luminion.sqlbooster.model.query.Sort;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.BeanPropertyUtils;
import io.github.luminion.sqlbooster.util.SqlContextUtils;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.function.BiFunction;

/**
 * SQL 构建器的抽象基类。
 * <p>
 * 提供了 SQL 构建的基本功能，包括条件添加、排序设置等，并通过泛型支持链式调用。
 *
 * @param <T> 实体类型
 * @param <S> 返回类型 (用于支持链式调用)
 */
@SuppressWarnings({"unused", "unchecked"})
@RequiredArgsConstructor
public abstract class AbstractSqlBuilder<T, S extends AbstractSqlBuilder<T, S>> {

    /**
     * 关联的实体类, 用于 SQL 校验和处理。
     */
    protected final Class<T> entityClass;
    /**
     * 存放条件的上下文。
     */
    protected final SqlContext<T> sqlContext = new SqlContext<>();

    /**
     * 创建一个新的自身实例，用于实现链式调用中的内部构建。
     */
    protected abstract S newInstance();

    public SqlContext<T> build() {
        return build(SqlContextUtils::build);
    }

    public SqlContext<T> build(BiFunction<Class<T>, SqlContext<T>, SqlContext<T>> builder) {
        return builder.apply(this.entityClass, this.sqlContext);
    }

    public S append(Condition condition) {
        if (condition != null) {
            this.sqlContext.getConditions().add(condition);
        }
        return (S) this;
    }

    public S append(Sort sort) {
        if (sort != null) {
            this.sqlContext.getSorts().add(sort);
        }
        return (S) this;
    }

    public S append(ConditionSegment conditionSegment) {
        if (conditionSegment != null) {
            this.sqlContext.merge(conditionSegment);
        }
        return (S) this;
    }

    /**
     * 将 Map 转换为多个 "等于" (EQ) 条件。
     * <p>
     * Map 的键作为字段名，值作为查询值。值为 null 的条目将被忽略。
     */
    public S appendEqByMap(Map<?, ?> map) {
        if (map != null) {
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                Object key = entry.getKey();
                Object value = entry.getValue();
                if (value != null) {
                    Condition condition = new Condition(key.toString(), SqlKeyword.EQ.getSymbol(), value);
                    this.sqlContext.getConditions().add(condition);
                }
            }
        }
        return (S) this;
    }


    /**
     * 将 JavaBean 对象的属性转换为多个 "等于" (EQ) 条件。
     * <p>
     * 属性值为 null 的将被忽略。
     *
     * @param bean 包含查询值的实体对象
     */
    public S appendEqByJavaBean(Object bean) {
        if (bean != null){
            Map<String, Object> stringObjectMap = BeanPropertyUtils.toMap(bean);
            this.appendEqByMap(stringObjectMap);
        }
        return (S) this;
    }

}
