package io.github.luminion.sqlbooster.model.helper;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.api.Sort;
import io.github.luminion.sqlbooster.model.api.Tree;
import io.github.luminion.sqlbooster.model.api.Wrapper;
import io.github.luminion.sqlbooster.util.BoostUtils;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.Getter;

import java.util.Map;

/**
 * SQL 构建助手抽象基类.
 * <p>
 * 提供了 SQL 构建的基本功能, 包括条件添加、排序设置等, 并通过泛型支持链式调用.
 *
 * @param <T> 实体类型
 * @param <S> 返回类型 (用于支持链式调用)
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings({"unused", "unchecked"})
public abstract class BaseHelper<T, S extends BaseHelper<T, S>>  extends AbstractHelper<T> {
    

    /**
     * 合并指定条件树的条件
     *
     * @param tree 条件树
     * @return 当前实例
     * @since 1.0.0
     */
    public S merge(Tree tree) {
        if (tree != null) {
            super.addChild(tree);
        }
        return (S) this;
    }

    /**
     * 合并一个查询条件
     *
     * @param condition 查询条件
     * @return 当前实例
     * @since 1.0.0
     */
    public S merge(Condition condition) {
        if (condition != null) {
            this.getConditions().add(condition);
        }
        return (S) this;
    }

    /**
     * 合并一个排序规则
     *
     * @param sort 排序规则
     * @return 当前实例
     * @since 1.0.0
     */
    public S merge(Sort sort) {
        if (sort != null) {
            this.getSorts().add(sort);
        }
        return (S) this;
    }

    /**
     * 从一个Map对象加载查询条件.
     * <p>
     * 会将Map中的所有键值对映射为等值查询条件.
     *
     * @param map 包含查询条件的Map对象
     * @return 当前实例
     * @since 1.0.0
     */
    public <K, V> S merge(Map<K, V> map) {
        for (Map.Entry<K, V> entry : map.entrySet()) {
            K key = entry.getKey();
            V value = entry.getValue();
            Condition condition = new Condition(key.toString(), value);
            this.getConditions().add(condition);
        }
        return (S) this;
    }

    /**
     * 从一个普通对象(如DTO)加载查询条件.
     * <p>
     * 会通过反射将对象的非空字段映射为等值查询条件.
     *
     * @param dto 包含查询条件的对象
     * @return 当前实例
     * @since 1.0.0
     */
    public S merge(Object dto) {
        if (dto == null) {
            return (S) this;
        }
        Map<?, ?> map = ReflectUtils.objectToMap(dto);
        return merge(map);
    }

    /**
     * 设置实体类
     *
     * @param booster Booster 实例
     * @return 当前实例
     * @since 1.0.0
     */
    public <V> S entity(Booster<T, V> booster) {
        this.entityClass = BoostUtils.getEntityClass(booster);
        return (S) this;
    }

    /**
     * 转换为 {@link SqlHelperBooster}.
     *
     * @param boosterCore {@link BoosterCore} 实例
     * @param <V>       VO 类型
     * @param <P>       分页对象类型
     * @return {@link SqlHelperBooster} 实例
     * @since 1.0.0
     */
    public <V, P> SqlHelperBooster<T, V> boost(BoosterCore<T, V> boosterCore) {
        this.entityClass = BoostUtils.getEntityClass(boosterCore);
        return new SqlHelperBooster<>(boosterCore,this);
    }

}