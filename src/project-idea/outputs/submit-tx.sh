cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --change-address $(cat /home/gandalfg/Cardano/plutus-vasil/v2-contracts/test-address/payment.addr) \
    --tx-in bf5758db1c579fa96b75f88c074d32c7bbebc9afe623af87dad2d75a727fd698#0 \
    --tx-out $(cat /home/gandalfg/Cardano/plutus-vasil/v2-contracts/escrow.plutus)+15000000 \
    --tx-out-datum-embed-file /home/gandalfg/Cardano/plutus-vasil/v2-contracts/test-datum.json \
    --required-signer-hash 27d2fc0931ec6164d8c353bef704edccf526f521f8f7988d8f1606cd \
    --protocol-params-file /home/gandalfg/Cardano/plutus-vasil/v2-contracts/test-address/protocol.json \
	--out-file "/home/gandalfg/Cardano/plutus-vasil/v2-contracts/src/project-idea/outputs/full-tx.body"


cardano-cli transaction sign \
   --tx-body-file src/project-dea/outputs/full-tx.body \
   --testnet-magic 1 \
   --signing-key-file ~/Cardano/plutus-vasil/plutus-mint-lock-unlock-dapp/test-addresses/01.skey \
   --out-file tx-files/full-tx.signed

cardano-cli transaction submit --tx-file tx-files/full-tx.signed --testnet-magic 1