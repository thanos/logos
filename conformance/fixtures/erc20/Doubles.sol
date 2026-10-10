// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/// Independent ERC-20 doubles for the execution harness.
/// These are not Logos output. They exist so the runner can observe
/// return data, reverts, balances, and callbacks on a local chain.

contract LedgerToken {
    mapping(address => uint256) public balanceOf;
    mapping(address => mapping(address => uint256)) public allowance;

    constructor() {
        balanceOf[msg.sender] = 1000;
    }

    function approve(address spender, uint256 amount) external returns (bool) {
        allowance[msg.sender][spender] = amount;
        return true;
    }

    function transferFrom(address from, address to, uint256 amount) external returns (bool) {
        require(to != address(0), "zero recipient");
        require(amount != 0, "zero amount");
        require(balanceOf[from] >= amount, "balance");
        require(allowance[from][msg.sender] >= amount, "allowance");
        balanceOf[from] -= amount;
        balanceOf[to] += amount;
        allowance[from][msg.sender] -= amount;
        return true;
    }
}

contract TokenTrue {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        return true;
    }
}

contract TokenFalse {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        return false;
    }
}

contract TokenEmpty {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        assembly {
            return(0, 0)
        }
    }
}

contract TokenRevert {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        revert("nope");
    }
}

contract TokenOneByte {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        assembly {
            mstore(0, 1)
            return(0, 1)
        }
    }
}

contract TokenThirtyOne {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        assembly {
            mstore(0, 1)
            return(0, 31)
        }
    }
}

contract TokenThirtyThree {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        assembly {
            mstore(0, 1)
            mstore(32, 1)
            return(0, 33)
        }
    }
}

contract TokenDirtyBool {
    function transferFrom(address, address, uint256) external pure returns (bool) {
        assembly {
            mstore(0, 2)
            return(0, 32)
        }
    }
}

contract CallbackToken {
    function transferFrom(address, address, uint256) external returns (bool) {
        (bool ok, ) = msg.sender.call(abi.encodeWithSignature("onToken()"));
        require(ok, "callback failed");
        return true;
    }
}

contract ReentrantVictim {
    uint256 public calls;

    function onToken() external {
        calls += 1;
    }

    function pull(address token) external {
        (bool ok, ) = token.call(
            abi.encodeWithSignature(
                "transferFrom(address,address,uint256)",
                msg.sender,
                address(this),
                1
            )
        );
        require(ok, "pull failed");
    }
}
