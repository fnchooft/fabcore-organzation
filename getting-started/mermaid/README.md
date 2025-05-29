

# Mermaid digrams

During a recent AI-course AI, the topic or re-factoring came up.
Specifically to convert images or drawing to mermaid-syntax.

So I took that opertunity to:

1. Give Gemini an image
2. Ask it to convert it to mermaid syntax

## Result

```mermaid
%%{init: {'theme':'dark'}}%%
graph TD
    AAA_Root["AAA"]

    %% Authentication Branch
    Node_AuthN["Authentication"]
    AAA_Root --> Node_AuthN

    subgraph S_Authentication [Authentication]
        Node_AuthN_GroupsContainer["Groups (container)"]
        Node_AuthN_GroupAdmin["Group<br/>name=&quot;admin&quot;<br/>users=&quot;joe&quot;"]
        Node_AuthN_GroupLamers["Group<br/>name=&quot;lamers&quot;<br/>users=&quot;steve&quot;"]
        Node_AuthN_UsersContainer["Users (container)"]
        Node_AuthN_UserJoe["User<br/>name=&quot;joe&quot;<br/>password=&quot;xyz&quot;"]
        Node_AuthN_UserSteve["User<br/>name=&quot;steve&quot;<br/>password=&quot;zyx&quot;"]
    end
    Node_AuthN --> Node_AuthN_GroupsContainer
    Node_AuthN_GroupsContainer --> Node_AuthN_GroupAdmin
    Node_AuthN_GroupsContainer --> Node_AuthN_GroupLamers
    Node_AuthN --> Node_AuthN_UsersContainer
    Node_AuthN_UsersContainer --> Node_AuthN_UserJoe
    Node_AuthN_UsersContainer --> Node_AuthN_UserSteve

    %% Authorization Branch
    Node_AuthZ["Authorization"]
    AAA_Root --> Node_AuthZ

    subgraph S_Authorization [Authorization]
        Node_AuthZ_GroupsContainer["Groups (container)"]

        Node_AuthZ_GroupAdmin["Group<br/>name=&quot;admin&quot;"]
        Node_AuthZ_Admin_CLI["CLI"]
        Node_AuthZ_Admin_CLI_PathsContainer["paths (container)"]
        Node_AuthZ_Admin_CLI_PathExecute["path=/ <br/>access=execute"]

        Node_AuthZ_Admin_Netconf["netconf"]
        Node_AuthZ_Admin_Netconf_PathsContainer["paths (container)"]
        Node_AuthZ_Admin_Netconf_PathExecute["path=/ <br/>access=execute"]


        Node_AuthZ_GroupLamers["Group<br/>name=&quot;lamers&quot;"]
        Node_AuthZ_Lamers_Netconf["netconf"]
        Node_AuthZ_Lamers_Netconf_PathsContainer["paths (container)"]
        Node_AuthZ_Lamers_Netconf_PathDhcpWrite["path=/dhcp <br/>access=write"]
        Node_AuthZ_Lamers_Netconf_PathAaaRead["path=/aaa <br/>access=read"]
    end
    Node_AuthZ --> Node_AuthZ_GroupsContainer

    Node_AuthZ_GroupsContainer --> Node_AuthZ_GroupAdmin
    Node_AuthZ_GroupAdmin --> Node_AuthZ_Admin_CLI
    Node_AuthZ_Admin_CLI --> Node_AuthZ_Admin_CLI_PathsContainer
    Node_AuthZ_Admin_CLI_PathsContainer --> Node_AuthZ_Admin_CLI_PathExecute

    Node_AuthZ_GroupAdmin --> Node_AuthZ_Admin_Netconf
    Node_AuthZ_Admin_Netconf --> Node_AuthZ_Admin_Netconf_PathsContainer
    Node_AuthZ_Admin_Netconf_PathsContainer --> Node_AuthZ_Admin_Netconf_PathExecute

    Node_AuthZ_GroupsContainer --> Node_AuthZ_GroupLamers
    Node_AuthZ_GroupLamers --> Node_AuthZ_Lamers_Netconf
    Node_AuthZ_Lamers_Netconf --> Node_AuthZ_Lamers_Netconf_PathsContainer
    Node_AuthZ_Lamers_Netconf_PathsContainer --> Node_AuthZ_Lamers_Netconf_PathDhcpWrite
    Node_AuthZ_Lamers_Netconf_PathsContainer --> Node_AuthZ_Lamers_Netconf_PathAaaRead
```

## Experience

Great! That saves us a lot of time - and we do not have to ask a junior to do
this for us.

The result is pretty good, and other advantages - such as diffing the code
become easier this way.

## GitBook

- An example in the GitBook-docs itself would not hurt anyone... ;)

- Dark-mode - would really be nice ( if it worked in GitBook).

## Admonitions

You need to use hints... not very pretty...


{% hint style="info" %}
**Info hints** are great for showing general information, or providing tips and tricks.
{% endhint %}

{% hint style="success" %}
**Success hints** are good for showing positive actions or achievements.
{% endhint %}

{% hint style="warning" %}
**Warning hints** are good for showing important information or non-critical warnings.
{% endhint %}

{% hint style="danger" %}
**Danger hints** are good for highlighting destructive actions or raising attention to critical information.
{% endhint %}




## Links

- [Mermaid Gitbook Examples](https://raw.githubusercontent.com/mermaidjs/mermaid-gitbook)